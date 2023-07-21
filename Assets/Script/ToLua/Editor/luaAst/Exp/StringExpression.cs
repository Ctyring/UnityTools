namespace Script.ToLua.Editor.luaAst
{
    public class StringExpression: ValExpression
    {
        public string OpenTag = LuaDefine.Tokens.Quote;
        public IdentifierNameExpression content;
        public string CloseTag = LuaDefine.Tokens.Quote;

        public StringExpression(IdentifierNameExpression identifier) : base(identifier.name, identifier) {
            content = identifier;
        }
    }
}