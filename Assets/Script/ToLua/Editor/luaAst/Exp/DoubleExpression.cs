namespace Script.ToLua.Editor.luaAst {
    public class DoubleExpression: ValExpression {
        public double value;
        public DoubleExpression(double value) : base(value.ToString(), new IdentifierNameExpression(value.ToString())) {
            this.value = value;
        }
    }
}