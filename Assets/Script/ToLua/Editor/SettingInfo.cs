using System.Collections.Generic;

namespace Script.ToLua.Editor
{
    public sealed class SettingInfo {
        public bool HasSemicolon { get; set; }
        private int indent_;
        public string IndentString { get; private set; }
        public bool IsClassic { get; set; }
        public bool IsExportMetadata { get; set; }
        public string BaseFolder { get; set; } = "";
        public bool IsExportAttributesAll { get; private set; }
        public bool IsExportEnumAll { get; private set; }
        public bool IsModule { get; set; }
        public HashSet<string> ExportAttributes { get; private set; }
        public HashSet<string> ExportEnums { get; private set; }
        public HashSet<string> LuaModuleLibs;
        public bool IsInlineSimpleProperty { get; set; }
        public bool IsPreventDebugObject { get; set; }
        public bool IsNotConstantForEnum { get; set; }
        public bool IsNoConcurrent { get; set; }

        public SettingInfo() {
            Indent = 2;
        }

        public string[] Attributes {
            set {
                if (value != null) {
                    if (value.Length == 0) {
                        IsExportAttributesAll = true;
                    } else {
                        ExportAttributes = new HashSet<string>(value);
                    }
                }
            }
        }

        public string[] Enums {
            set {
                if (value != null) {
                    if (value.Length == 0) {
                        IsExportEnumAll = true;
                    } else {
                        ExportEnums = new HashSet<string>(value);
                    }
                }
            }
        }

        public int Indent {
            get {
                return indent_;
            }
            set {
                if (value > 0 && indent_ != value) {
                    indent_ = value;
                    IndentString = new string(' ', indent_);
                }
            }
        }
    }
}