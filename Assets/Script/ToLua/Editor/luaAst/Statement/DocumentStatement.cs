using System;

namespace Script.ToLua.Editor.luaAst {
    public class DocumentStatement : Statement {
        private const string kAttributePrefix = "@CSharpLua.";
        
        [Flags]
        public enum AttributeFlags {
            None = 0,
            Ignore = 1 << 0,
            NoField = 1 << 1,
            Metadata = 1 << 2,
            MetadataAll = 1 << 3,
            Template = 1 << 4,
            Params = 1 << 5,
        }
        
        public AttributeFlags attr_;
        
        public static string ToString(AttributeFlags attribute) {
            return kAttributePrefix + attribute;
        }
    }
}