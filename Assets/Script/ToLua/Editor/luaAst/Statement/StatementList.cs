namespace Script.ToLua.Editor.luaAst {
    public class StatementList: Statement {
        public LuaSyntaxList<Statement> Statements = new();
    }
}