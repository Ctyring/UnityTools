using Script.ToLua.Editor.luaAst.Exp;

namespace Script.ToLua.Editor.luaAst
{
    public class StringExpression: LiteralExpression
    {
        public string OpenTag = LuaDefine.Tokens.Quote;
        public IdentifierNameExpression content;
        public string CloseTag = LuaDefine.Tokens.Quote;

        public StringExpression(IdentifierNameExpression identifier) {
            content = identifier;
        }

        public override string Text {
            get {
                return content.name;
            }
        }

        public static readonly StringExpression Empty = new(LuaDefine.IdentifierName.Empty);
    }
}