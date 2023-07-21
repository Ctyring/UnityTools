namespace Script.ToLua.Editor.luaAst {
    public class BlankLineStatement: Statement {
        public int lineCount;
        public BlankLineStatement(int lineCount) {
            this.lineCount = lineCount;
        }
    }
}