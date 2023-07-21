namespace Script.ToLua.Editor.luaAst {
    public class ValExpression : Expression {
        public string value;
        public IdentifierNameExpression identifierName;
        
        public ValExpression(string value, IdentifierNameExpression identifierName) {
            this.value = value;
            this.identifierName = identifierName;
        }
    }
}