namespace Script.ToLua.Editor.luaAst {
    public class ConstExpression : ValExpression {
        public ConstExpression(string value, IdentifierNameExpression identifierName) : base(value, identifierName) {
        }
    }
}