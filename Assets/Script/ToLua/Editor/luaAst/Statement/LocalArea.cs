namespace Script.ToLua.Editor.luaAst {
    public class LocalArea: Statement {
        public string LocalKeyword => LuaDefine.Keyword.Local;
        public readonly LuaSyntaxList<IdentifierNameExpression> Variables = new();
    }
}