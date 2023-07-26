namespace Script.ToLua.Editor.luaAst.Exp {
    public abstract class NumberExpression: LiteralExpression {
        public abstract double Number { get; }
        public static readonly NumberExpression Zero = 0;
        public static readonly NumberExpression ZeroFloat = 0.0;
        
        public static implicit operator NumberExpression(float number) {
            return new FloatExpression(number);
        }

        public static implicit operator NumberExpression(double number) {
            return new DoubleExpression(number);
        }
    }
}