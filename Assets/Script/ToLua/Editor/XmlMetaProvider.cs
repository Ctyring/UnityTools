using System;
using System.Collections.Generic;
using System.IO;
using System.Xml.Serialization;
using Microsoft.CodeAnalysis;

namespace Script.ToLua.Editor {
    public class XmlMetaProvider {
        public class XmlMetaModel {
            enum MethodMetaType {
                Name,
                CodeTemplate,
                IgnoreGeneric,
            }

            public class TemplateModel {
                [XmlAttribute] public string Template;
            }


            public class MemberModel {
                [XmlAttribute] public string name;

                [XmlAttribute] public string Baned;

                protected static bool ParseBool(string v, out bool b) {
                    b = false;
                    if (v != null) {
                        if (v.Equals(bool.TrueString, StringComparison.OrdinalIgnoreCase)) {
                            b = true;
                            return true;
                        }

                        if (v.Equals(bool.FalseString, StringComparison.OrdinalIgnoreCase)) {
                            b = false;
                            return false;
                        }
                    }

                    return false;
                }

                public bool IsBaned {
                    get {
                        if (!string.IsNullOrEmpty(Baned)) {
                            if (ParseBool(Baned, out bool b)) {
                                return b;
                            }

                            return true;
                        }

                        return false;
                    }
                }

                private string BanedMessage {
                    get {
                        if (!string.IsNullOrEmpty(Baned)) {
                            if (ParseBool(Baned, out bool b)) {
                                return b ? "cannot use" : null;
                            }

                            return Baned;
                        }

                        return null;
                    }
                }

                public void CheckBaned(ISymbol symbol) {
                    if (IsBaned) {
                        throw new Exception("symbol " + symbol.Name + " is baned, " + BanedMessage);
                    }
                }
            }

            public class PropertyModel : MemberModel {
                [XmlAttribute] public string Name;
                [XmlElement] public XmlMetaModel.TemplateModel set;
                [XmlElement] public XmlMetaModel.TemplateModel get;
                [XmlAttribute] public string IsField;

                public bool? CheckIsField {
                    get {
                        if (ParseBool(IsField, out bool b)) {
                            return b;
                        }

                        return null;
                    }
                }
            }

            public class FieldModel : MemberModel {
                [XmlAttribute] public string Template;
                [XmlAttribute] public bool IsProperty;
            }

            public class ArgumentModel {
                [XmlAttribute] public string type;
                [XmlElement("arg")] public ArgumentModel[] GenericArgs;
            }

            public class MethodModel : MemberModel {
                [XmlAttribute] public string Name;
                [XmlAttribute] public string Template;
                [XmlAttribute] public int ArgCount = -1;
                [XmlElement("arg")] public ArgumentModel[] Args;
                [XmlAttribute] public string RetType;
                [XmlAttribute] public int GenericArgCount = -1;
                [XmlAttribute] public bool IgnoreGeneric;

                string GetMetaInfo(MethodMetaType type) {
                    switch (type) {
                        case MethodMetaType.Name: {
                            return Name;
                        }
                        case MethodMetaType.CodeTemplate: {
                            return Template;
                        }
                        case MethodMetaType.IgnoreGeneric: {
                            return IgnoreGeneric ? bool.TrueString : bool.FalseString;
                        }
                        default: {
                            throw new InvalidOperationException();
                        }
                    }
                }
            }

            public sealed class ClassModel : MemberModel {
                [XmlAttribute] public string Name;
                [XmlElement("property")] public PropertyModel[] Properties;
                [XmlElement("field")] public FieldModel[] Fields;
                [XmlElement("method")] public MethodModel[] Methods;
                [XmlAttribute] public bool IgnoreGeneric;
                [XmlAttribute] public bool Readonly;
            }

            public sealed class NamespaceModel : MemberModel {
                [XmlAttribute] public string Name;
                [XmlElement("class")] public ClassModel[] Classes;
            }

            public sealed class AssemblyModel {
                [XmlElement("namespace")] public NamespaceModel[] Namespaces;
                [XmlElement("class")] public ClassModel[] Classes;
            }

            [XmlElement("assembly")] public AssemblyModel Assembly;
        }

        public Dictionary<string, XmlMetaModel.NamespaceModel> namespaceNameMaps =
            new Dictionary<string, XmlMetaModel.NamespaceModel>();

        public string GetNamespaceMapName(INamespaceSymbol symbol, string original) {
            XmlMetaModel.NamespaceModel info = null;
            if (namespaceNameMaps.TryGetValue(original, out XmlMetaModel.NamespaceModel model)) {
                info = model;
            }

            if (info != null) {
                info.CheckBaned(symbol);
                return info.Name;
            }

            return null;
        }

        public XmlMetaProvider(IEnumerable<Stream> streams) {
            foreach (Stream stream in streams) {
                var serializer = new XmlSerializer(typeof(XmlMetaModel));
                XmlMetaModel model = (XmlMetaModel)serializer.Deserialize(stream);
                var assembly = model.Assembly;
                if (assembly != null) {
                    if (assembly.Namespaces != null) {
                        foreach (var namespaceModel in assembly.Namespaces) {
                            LoadNamespace(namespaceModel);
                        }
                    }

                    if (assembly.Classes != null) {
                        LoadType(string.Empty, assembly.Classes);
                    }
                }
            }
        }

        private void LoadNamespace(XmlMetaModel.NamespaceModel model) {
            string namespaceName = model.name;
            if (namespaceName == null) {
                throw new ArgumentException("namespace.name is null");
            }

            if (namespaceName.Length > 0) {
                if (namespaceNameMaps.ContainsKey(namespaceName)) {
                    throw new ArgumentException($"namespace [{namespaceName}] is already has");
                }

                if (!string.IsNullOrEmpty(model.Name) || model.IsBaned) {
                    namespaceNameMaps.Add(namespaceName, model);
                }
            }

            if (model.Classes != null) {
                string name = !string.IsNullOrEmpty(model.Name) ? model.Name : namespaceName;
                LoadType(name, model.Classes);
            }
        }
    }
}