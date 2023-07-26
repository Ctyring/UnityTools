namespace Script.ToLua.Editor.luaAst.Exp {
    public abstract class LiteralExpression : Expression {
        public abstract string Text { get; }
    }
}