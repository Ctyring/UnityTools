/*
Copyright 2017 YANG Huan (sy.yanghuan@gmail.com).

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml.Serialization;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Script.ToLua.Editor;
using Script.ToLua.Editor.luaAst;

namespace CSharpLua {
    public sealed class XmlMetaProvider {
        [XmlRoot("meta")]
        public sealed class XmlMetaModel {
            public sealed class TemplateModel {
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

                internal bool IsBaned {
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
                        throw new Exception("[XmlMetaProvider] is baned");
                    }
                }
            }

            public sealed class PropertyModel : MemberModel {
                [XmlAttribute] public string Name;
                [XmlElement] public TemplateModel set;
                [XmlElement] public TemplateModel get;
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

            public sealed class FieldModel : MemberModel {
                [XmlAttribute] public string Template;
                [XmlAttribute] public bool IsProperty;
            }

            public sealed class ArgumentModel {
                [XmlAttribute] public string type;
                [XmlElement("arg")] public ArgumentModel[] GenericArgs;
            }

            public sealed class MethodModel : MemberModel {
                [XmlAttribute] public string Name;
                [XmlAttribute] public string Template;
                [XmlAttribute] public int ArgCount = -1;
                [XmlElement("arg")] public ArgumentModel[] Args;
                [XmlAttribute] public string RetType;
                [XmlAttribute] public int GenericArgCount = -1;
                [XmlAttribute] public bool IgnoreGeneric;

                internal string GetMetaInfo(MethodMetaType type) {
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

        internal enum MethodMetaType {
            Name,
            CodeTemplate,
            IgnoreGeneric,
        }

        private sealed class MethodMetaInfo {
            private readonly List<XmlMetaModel.MethodModel> models_ = new();
            private bool isSingleModel_;

            public void Add(XmlMetaModel.MethodModel model) {
                models_.Add(model);
                CheckIsSingleModel();
            }

            private void CheckIsSingleModel() {
                bool isSingle = false;
                if (models_.Count == 1) {
                    var model = models_[0];
                    if (model.ArgCount == -1 && model.Args == null && model.RetType == null &&
                        model.GenericArgCount == -1) {
                        isSingle = true;
                    }
                }

                isSingleModel_ = isSingle;
            }

            private static string GetTypeString(ITypeSymbol symbol) {
                if (symbol.Kind == SymbolKind.TypeParameter) {
                    return symbol.Name;
                }

                StringBuilder sb = new StringBuilder();
                INamedTypeSymbol typeSymbol = (INamedTypeSymbol)symbol.OriginalDefinition;
                var namespaceSymbol = typeSymbol.ContainingNamespace;

                if (symbol.ContainingType != null) {
                    sb.Append(GetTypeString(symbol.ContainingType));
                    sb.Append('.');
                }
                else if (!namespaceSymbol.IsGlobalNamespace) {
                    sb.Append(namespaceSymbol);
                    sb.Append('.');
                }

                sb.Append(symbol.Name);
                if (typeSymbol.TypeArguments.Length > 0) {
                    sb.Append('`');
                    sb.Append(typeSymbol.TypeArguments.Length);
                }

                return sb.ToString();
            }

            private static bool IsTypeMatch(ITypeSymbol symbol, string typeString) {
                if (symbol.Kind == SymbolKind.ArrayType) {
                    var typeSymbol = (IArrayTypeSymbol)symbol;
                    string elementTypeName = GetTypeString(typeSymbol.ElementType);
                    return elementTypeName + "[]" == typeString;
                }

                string name = GetTypeString(symbol);
                return name == typeString;
            }

            private static bool IsArgMatch(ITypeSymbol symbol, XmlMetaModel.ArgumentModel parameterModel) {
                if (!IsTypeMatch(symbol, parameterModel.type)) {
                    return false;
                }

                if (parameterModel.GenericArgs != null) {
                    var typeSymbol = (INamedTypeSymbol)symbol;
                    if (typeSymbol.TypeArguments.Length != parameterModel.GenericArgs.Length) {
                        return false;
                    }

                    int index = 0;
                    foreach (var typeArgument in typeSymbol.TypeArguments) {
                        var genericArgModel = parameterModel.GenericArgs[index];
                        if (!IsArgMatch(typeArgument, genericArgModel)) {
                            return false;
                        }

                        ++index;
                    }
                }

                return true;
            }

            private bool IsMethodMatch(XmlMetaModel.MethodModel model, IMethodSymbol symbol) {
                if (model.name != symbol.Name) {
                    return false;
                }

                if (model.ArgCount != -1) {
                    if (symbol.Parameters.Length != model.ArgCount) {
                        return false;
                    }
                }

                if (model.GenericArgCount != -1) {
                    if (symbol.TypeArguments.Length != model.GenericArgCount) {
                        return false;
                    }
                }

                if (!string.IsNullOrEmpty(model.RetType)) {
                    if (!IsTypeMatch(symbol.ReturnType, model.RetType)) {
                        return false;
                    }
                }

                if (model.Args != null) {
                    if (symbol.Parameters.Length != model.Args.Length) {
                        return false;
                    }

                    int index = 0;
                    foreach (var parameter in symbol.Parameters) {
                        var parameterModel = model.Args[index];
                        if (!IsArgMatch(parameter.Type, parameterModel)) {
                            return false;
                        }

                        ++index;
                    }
                }

                return true;
            }

            private XmlMetaModel.MethodModel GetMethodModel(IMethodSymbol symbol, bool isCheckBaned) {
                var methodModel = isSingleModel_
                    ? models_[0]
                    : models_.Find(i => IsMethodMatch(i, symbol));
                if (methodModel != null && isCheckBaned) {
                    methodModel.CheckBaned(symbol);
                }

                return methodModel;
            }

            public string GetMetaInfo(IMethodSymbol symbol, MethodMetaType type) {
                return GetMethodModel(symbol, type == MethodMetaType.CodeTemplate)?.GetMetaInfo(type);
            }
        }

        private sealed class TypeMetaInfo {
            private readonly XmlMetaModel.ClassModel model_;
            private readonly Dictionary<string, XmlMetaModel.FieldModel> fields_ = new();
            private readonly Dictionary<string, XmlMetaModel.PropertyModel> properties_ = new();
            private readonly Dictionary<string, MethodMetaInfo> methods_ = new();

            public TypeMetaInfo(XmlMetaModel.ClassModel model) {
                model_ = model;
                Field();
                Property();
                Method();
            }

            public XmlMetaModel.ClassModel Model {
                get { return model_; }
            }

            private void Field() {
                if (model_.Fields != null) {
                    foreach (var fieldModel in model_.Fields) {
                        if (string.IsNullOrEmpty(fieldModel.name)) {
                            throw new ArgumentException($"type [{model_.name}] has a field name is empty");
                        }

                        if (fields_.ContainsKey(fieldModel.name)) {
                            throw new ArgumentException(
                                $"type [{model_.name}]'s field [{fieldModel.name}] is already exists");
                        }

                        fields_.Add(fieldModel.name, fieldModel);
                    }
                }
            }

            private void Property() {
                if (model_.Properties != null) {
                    foreach (var propertyModel in model_.Properties) {
                        if (string.IsNullOrEmpty(propertyModel.name)) {
                            throw new ArgumentException($"type [{model_.name}] has a property name is empty");
                        }

                        if (fields_.ContainsKey(propertyModel.name)) {
                            throw new ArgumentException(
                                $"type [{model_.name}]'s property [{propertyModel.name}] is already exists");
                        }

                        properties_.Add(propertyModel.name, propertyModel);
                    }
                }
            }

            private void Method() {
                if (model_.Methods != null) {
                    foreach (var methodModel in model_.Methods) {
                        if (string.IsNullOrEmpty(methodModel.name)) {
                            throw new ArgumentException($"type [{model_.name}] has a method name is empty");
                        }

                        // var info = methods_.GetOrDefault(methodModel.name);
                        // if (methods_.TryGetValue(methodModel.name, out var info)) {}
                        MethodMetaInfo info = null;
                        if (methods_.TryGetValue(methodModel.name, out var value)) {
                            info = value;
                        }

                        if (info == null) {
                            info = new MethodMetaInfo();
                            methods_.Add(methodModel.name, info);
                        }

                        info.Add(methodModel);
                    }
                }
            }

            public XmlMetaModel.FieldModel GetFieldModel(string name) {
                if(fields_.TryGetValue(name, out var value))
                {
                    return value;
                }

                return default;
            }

            public XmlMetaModel.PropertyModel GetPropertyModel(string name) {
                if(properties_.TryGetValue(name, out var value))
                {
                    return value;
                }

                return default;
            }

            public MethodMetaInfo GetMethodMetaInfo(string name) {
                if(methods_.TryGetValue(name, out var value))
                {
                    return value;
                }

                return default;
            }
        }

        private readonly Dictionary<string, XmlMetaModel.NamespaceModel> namespaceNameMaps_ = new();
        private readonly Dictionary<string, TypeMetaInfo> typeMetas_ = new();

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
                if (namespaceNameMaps_.ContainsKey(namespaceName)) {
                    throw new ArgumentException($"namespace [{namespaceName}] is already has");
                }

                if (!string.IsNullOrEmpty(model.Name) || model.IsBaned) {
                    namespaceNameMaps_.Add(namespaceName, model);
                }
            }

            if (model.Classes != null) {
                string name = !string.IsNullOrEmpty(model.Name) ? model.Name : namespaceName;
                LoadType(name, model.Classes);
            }
        }

        private void LoadType(string namespaceName, XmlMetaModel.ClassModel[] classes) {
            foreach (var classModel in classes) {
                string className = classModel.name;
                if (string.IsNullOrEmpty(className)) {
                    throw new ArgumentException($"namespace [{namespaceName}] has a class's name is empty");
                }

                string classesFullName = namespaceName.Length > 0 ? namespaceName + '.' + className : className;
                classesFullName = classesFullName.Replace('`', '_');
                if (typeMetas_.ContainsKey(classesFullName)) {
                    throw new ArgumentException($"type [{classesFullName}] is already has");
                }

                TypeMetaInfo info = new TypeMetaInfo(classModel);
                typeMetas_.Add(classesFullName, info);
            }
        }

        public string GetNamespaceMapName(INamespaceSymbol symbol, string original) {
            XmlMetaModel.NamespaceModel info = null;
            if(namespaceNameMaps_.TryGetValue(original, out var value)) {
                info = value;
            }
            if (info != null) {
                info.CheckBaned(symbol);
                return info.Name;
            }

            return null;
        }

        /// <summary>
        /// 判断是否可能有元数据
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        bool MayHaveCodeMeta(ISymbol symbol) {
            return symbol.DeclaredAccessibility == Accessibility.Public && symbol.DeclaringSyntaxReferences.IsEmpty;
        }

        private string GetTypeShortString(ISymbol symbol) {
            INamedTypeSymbol typeSymbol = (INamedTypeSymbol)symbol.OriginalDefinition;
            
            return GetTypeShortName(typeSymbol, GetNamespaceMapName);
        }
        
        public static string GetTypeShortName(
            INamedTypeSymbol typeSymbol,
            Func<INamespaceSymbol, string, string> funcOfNamespace = null,
            Func<INamedTypeSymbol, string> funcOfTypeName = null,
            LuaSyntaxTreeBuilder transfor = null) {
            StringBuilder sb = new StringBuilder();
            FillExternalTypeName(sb, typeSymbol, funcOfNamespace, funcOfTypeName, transfor);
            string typeName = funcOfTypeName?.Invoke(typeSymbol);
            if (typeName != null) {
                sb.Append(typeName);
            } else {
                typeName = typeSymbol.Name;
                sb.Append(typeName);
                int typeParametersCount = typeSymbol.TypeParameters.Length;
                if (typeParametersCount > 0) {
                    sb.Append('_');
                    sb.Append(typeParametersCount);
                }
            }
            return sb.ToString();
        }
        
        public static void FillExternalTypeName(
            StringBuilder sb,
            INamedTypeSymbol typeSymbol,
            Func<INamespaceSymbol, string, string> funcOfNamespace,
            Func<INamedTypeSymbol, string> funcOfTypeName,
            LuaSyntaxTreeBuilder transfor = null) {
            var externalType = typeSymbol.ContainingType;
            if (externalType != null) {
                if (transfor is {IsNoneGenericTypeCounter: true} && !externalType.IsGenericType && !typeSymbol.IsGenericType) {
                    var curTypeDeclaration = transfor.CurTypeDeclaration;
                    if (curTypeDeclaration != null && curTypeDeclaration.CheckTypeName(externalType, out var classIdentifier)) {
                        sb.Append(classIdentifier.name);
                        sb.Append('.');
                        return;
                    }
                }

                FillExternalTypeName(sb, externalType, funcOfNamespace, funcOfTypeName);
                string typeName = funcOfTypeName?.Invoke(typeSymbol) ?? externalType.Name;
                sb.Append(typeName);
                int typeParametersCount = externalType.TypeParameters.Length;
                if (typeParametersCount > 0) {
                    sb.Append('_');
                    sb.Append(typeParametersCount);
                }
                sb.Append('.');
            } else {
                FillNamespaceName(sb, typeSymbol, funcOfNamespace);
            }
        }
        
        private static void FillNamespaceName(StringBuilder sb, INamedTypeSymbol typeSymbol, Func<INamespaceSymbol, string, string> funcOfNamespace) {
            string namespaceName;
            var namespaceSymbol = typeSymbol.ContainingNamespace;
            if (namespaceSymbol.IsGlobalNamespace) {
                namespaceName = string.Empty;
            } else {
                namespaceName = namespaceSymbol.ToString();
                string newName = funcOfNamespace?.Invoke(namespaceSymbol, namespaceName);
                if (newName != null) {
                    namespaceName = newName;
                }
            }
            if (namespaceName.Length > 0) {
                sb.Append(namespaceName);
                sb.Append('.');
            }
        }

        /// <summary>
        /// 从xml中查找typename
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="shortName"></param>
        /// <returns></returns>
        public string GetTypeMapName(ISymbol symbol, string shortName) {
            // 如果可能有元数据，就去查找
            if (MayHaveCodeMeta(symbol)) {
                var info = GetTypeMetaInfo(symbol, shortName);
                return info?.Model.Name;
            }

            return null;
        }

        public bool IsTypeIgnoreGeneric(INamedTypeSymbol typeSymbol) {
            if (MayHaveCodeMeta(typeSymbol)) {
                var info = GetTypeMetaInfo(typeSymbol);
                return info != null && info.Model.IgnoreGeneric;
            }

            return false;
        }

        internal bool IsTypeReadOnly(INamedTypeSymbol typeSymbol) {
            if (MayHaveCodeMeta(typeSymbol)) {
                var info = GetTypeMetaInfo(typeSymbol);
                return info != null && info.Model.Readonly;
            }

            return false;
        }

        private TypeMetaInfo GetTypeMetaInfo(ISymbol symbol, string shortName) {
            TypeMetaInfo info = default;
            if (typeMetas_.TryGetValue(shortName, out var value)) {
                info = value;
            }
            info?.Model.CheckBaned(symbol);
            return info;
        }

        private TypeMetaInfo GetTypeMetaInfo(INamedTypeSymbol typeSymbol) {
            string shortName = GetTypeShortString(typeSymbol);
            return GetTypeMetaInfo(typeSymbol, shortName);
        }

        private TypeMetaInfo GetTypeMetaInfo(ISymbol memberSymbol) {
            return GetTypeMetaInfo(memberSymbol.ContainingType);
        }

        private XmlMetaModel.FieldModel GetFieldMetaInfo(IFieldSymbol symbol) {
            if (MayHaveCodeMeta(symbol)) {
                return GetTypeMetaInfo(symbol)?.GetFieldModel(symbol.Name);
            }

            return null;
        }

        public string GetFieldCodeTemplate(IFieldSymbol symbol) {
            return GetFieldMetaInfo(symbol)?.Template ?? GetCodeTemplateFromAttribute(symbol);
        }

        public bool IsFieldForceProperty(IFieldSymbol symbol) {
            return GetFieldMetaInfo(symbol)?.IsProperty ?? false;
        }

        private XmlMetaModel.PropertyModel GetPropertyMetaInfo(IPropertySymbol symbol) {
            if (MayHaveCodeMeta(symbol)) {
                var info = GetTypeMetaInfo(symbol)?.GetPropertyModel(symbol.Name);
                info?.CheckBaned(symbol);
                return info;
            }

            return null;
        }

        public bool? IsPropertyField(IPropertySymbol symbol) {
            return GetPropertyMetaInfo(symbol)?.CheckIsField;
        }

        public string GetPropertyCodeTemplate(IPropertySymbol symbol, bool isGet) {
            var info = GetPropertyMetaInfo(symbol);
            if (info != null) {
                return isGet ? info.get?.Template : info.set?.Template;
            }

            return null;
        }

        public string GetPropertyMapName(IPropertySymbol symbol) {
            return GetPropertyMetaInfo(symbol)?.Name;
        }

        /// <summary>
        /// 获取内部方法的元信息
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="metaType"></param>
        /// <returns></returns>
        private string GetInternalMethodMetaInfo(IMethodSymbol symbol, MethodMetaType metaType) {
            Contract.Assert(symbol != null);
            if (symbol.DeclaredAccessibility != Accessibility.Public) {
                return null;
            }

            string metaInfo = null;
            // 没有声明该方法的语法节点，从xml文件里面查
            if (symbol.DeclaringSyntaxReferences.IsEmpty) {
                metaInfo = GetTypeMetaInfo(symbol)?.GetMethodMetaInfo(symbol.Name)?.GetMetaInfo(symbol, metaType);
            }
            
            // 如果没有找到元方法，尝试从父类或者接口中查找
            if (metaInfo == null) {
                // 如果是重写方法，从父类中查找
                if (symbol.IsOverride) {
                    if (symbol.OverriddenMethod != null) {
                        metaInfo = GetInternalMethodMetaInfo(symbol.OverriddenMethod, metaType);
                    }
                }
                else {
                    // 获取该symbol的所有接口实现
                    var interfaceImplementations = InterfaceImplementations(symbol);
                    if (interfaceImplementations != null) {
                        foreach (IMethodSymbol interfaceMethod in interfaceImplementations) {
                            // 如果是接口实现，从接口中查找模板
                            metaInfo = GetInternalMethodMetaInfo(interfaceMethod, metaType);
                            if (metaInfo != null) {
                                break;
                            }
                        }
                    }
                }
            }

            return metaInfo;
        }
        
        /// <summary>
        /// 找到symbol的接口实现
        /// </summary>
        /// <param name="symbol"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static IEnumerable<T> InterfaceImplementations<T>(T symbol) where T : ISymbol {
            if (!symbol.IsStatic) {
                var type = symbol.ContainingType;
                if (type != null) {
                    var interfaceSymbols = type.AllInterfaces.SelectMany(i => i.GetMembers().OfType<T>());
                    return interfaceSymbols.Where(i => symbol.Equals(type.FindImplementationForInterfaceMember(i)));
                }
            }
            return Array.Empty<T>();
        }

        /// <summary>
        /// 获取代码的元信息
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="metaType"></param>
        /// <returns></returns>
        private string GetMethodMetaInfo(IMethodSymbol symbol, MethodMetaType metaType) {
            // 获取原始方法
            CheckMethodDefinition(ref symbol);
            return GetInternalMethodMetaInfo(symbol, metaType);
        }
        
        public static void CheckMethodDefinition(ref IMethodSymbol symbol) {
            // 判断是否为扩展方法
            if (symbol.IsExtensionMethod) {
                // 判断是否为简化的扩展方法
                if (symbol.ReducedFrom != null && !SymbolEqualityComparer.Default.Equals(symbol, symbol.ReducedFrom)) {
                    symbol = symbol.ReducedFrom;
                } else {
                    CheckSymbolDefinition(ref symbol);
                }
            } else {
                CheckSymbolDefinition(ref symbol);
            }
        }
        
        // 获取原始方法
        private static void CheckSymbolDefinition<T>(ref T symbol) where T : class, ISymbol {
            var originalDefinition = (T)symbol.OriginalDefinition;
            if (originalDefinition != symbol) {
                symbol = originalDefinition;
            }
        }

        public string GetMethodMapName(IMethodSymbol symbol) {
            return GetMethodMetaInfo(symbol, MethodMetaType.Name);
        }

        /// <summary>
        /// 获取方法的代码模板
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public string GetMethodCodeTemplate(IMethodSymbol symbol) {
            // 前者是有定义，后者是无定义只声明
            return GetMethodMetaInfo(symbol, MethodMetaType.CodeTemplate) ?? GetCodeTemplateFromAttribute(symbol);
        }

        public bool IsMethodIgnoreGeneric(IMethodSymbol symbol) {
            return GetMethodMetaInfo(symbol, MethodMetaType.IgnoreGeneric) == bool.TrueString;
        }
        
        /// <summary>
        /// 查找Attribute设定的模板(主要转化extern标记的方法，结合注释)
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public static string GetCodeTemplateFromAttribute(ISymbol symbol) {
            var node = GetDeclaringSyntaxNode(symbol);
            // 如果有声明
            if (node != null) {
                // 如果是字段类型
                if (symbol.Kind == SymbolKind.Field) {
                    node = node.Parent.Parent;
                }
                if (HasCSharpLuaAttribute(node, DocumentStatement.AttributeFlags.Template, out string text)) {
                    return GetCodeTemplateFromAttributeText(text);
                }
            } else {
                // 如果没有声明，尝试从xml文件中查找
                string xml = symbol.GetDocumentationCommentXml();
                if (xml != null) {
                    return GetCodeTemplateFromAttributeText(xml);
                }
            }
            return null;
        }
        
        /// <summary>
        /// 获取给定符号的声明语法节点
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public static SyntaxNode GetDeclaringSyntaxNode(ISymbol symbol) {
            return symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
        }
        
        public static bool HasCSharpLuaAttribute(SyntaxNode node, DocumentStatement.AttributeFlags attribute, out string text) {
            text = null;
            var documentTrivia = node.GetLeadingTrivia().FirstOrDefault(i => i.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));
            if (documentTrivia != default) {
                string document = documentTrivia.ToString();
                if (document.Contains(DocumentStatement.ToString(attribute))) {
                    text = document;
                    return true;
                }
            }
            return false;
        }
        
        private static readonly Regex codeTemplateAttributeRegex_ = new(@"@CSharpLua.Template\s*=\s*(.+)\s*", RegexOptions.Compiled);
        
        private static string GetCodeTemplateFromAttributeText(string document) {
            var matches = codeTemplateAttributeRegex_.Matches(document);
            if (matches.Count > 0) {
                string text = matches[0].Groups[1].Value;
                return text.Trim().Trim('"');
            }
            return null;
        }
    }
}