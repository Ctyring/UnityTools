using Script.ToLua.Editor.luaAst.Exp;

namespace Script.ToLua.Editor.luaAst {
    public class IdentifierExpression: LiteralExpression {
        public IdentifierNameExpression identifierName;
        public IdentifierExpression(string text) : this((IdentifierNameExpression)text) {
        }

        public IdentifierExpression(IdentifierNameExpression identifier) {
            identifierName = identifier;
        }

        public override string Text {
            get {
                return identifierName.name;
            }
        }
        
        public static readonly IdentifierExpression Nil = new(LuaDefine.Keyword.Nil);
        public static readonly IdentifierExpression True = new(LuaDefine.Keyword.True);
        public static readonly IdentifierExpression False = new(LuaDefine.Keyword.False);
    }
}