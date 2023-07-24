namespace Script.ToLua.Editor.luaAst {
    public class ValExpression : Expression {
        public string value;
        public IdentifierNameExpression identifierName;
        public static ValExpression Zero = 0;
        public static ValExpression ZeroFloat = 0.0;
        
        public ValExpression(string value, IdentifierNameExpression identifierName) {
            this.value = value;
            this.identifierName = identifierName;
        }
        
        public static implicit operator ValExpression(float number) {
            return new FloatExpression(number);
        }

        public static implicit operator ValExpression(double number) {
            return new DoubleExpression(number);
        }
    }
}