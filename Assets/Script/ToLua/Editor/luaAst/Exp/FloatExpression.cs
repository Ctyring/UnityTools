namespace Script.ToLua.Editor.luaAst {
    public class FloatExpression : ValExpression {
        public float value;
        public FloatExpression(float value) : base(value.ToString(), new IdentifierNameExpression(value.ToString())) {
            this.value = value;
        }
    }
}