namespace Script.ToLua.Editor.luaAst {
    public class IdentifierExpression: ValExpression {
        public IdentifierNameExpression identifierName;
        public IdentifierExpression(string value) : base(value, new IdentifierNameExpression(value)) {
            identifierName = new IdentifierNameExpression(value);
        }
    }
}