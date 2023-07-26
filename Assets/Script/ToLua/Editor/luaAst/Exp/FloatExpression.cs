using System.Globalization;
using Script.ToLua.Editor.luaAst.Exp;

namespace Script.ToLua.Editor.luaAst {
    public class FloatExpression : Exp.NumberExpression {
        private readonly float number_;

        public FloatExpression(float number) {
            number_ = number;
        }

        public override double Number {
            get {
                return number_;
            }
        }

        public override string Text {
            get {
                return number_.ToString("G9", CultureInfo.InvariantCulture);
            }
        }
    }
}