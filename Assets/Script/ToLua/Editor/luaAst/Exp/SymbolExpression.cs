namespace Script.ToLua.Editor.luaAst {
    public class SymbolExpression: IdentifierNameExpression {
        public Expression NameExpression { get; private set; }

        public SymbolExpression(Expression identifierName) : base("") {
            NameExpression = identifierName;
        }

        public void Update(string newName) {
            NameExpression = newName;
        }
    }
}