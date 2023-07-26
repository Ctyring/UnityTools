using Script.ToLua.Editor.luaAst.Exp;

namespace Script.ToLua.Editor.luaAst {
    public class ConstExpression : LiteralExpression {
        public LiteralExpression Value { get; }
        public string OpenComment => Tokens.OpenLongComment;
        public string IdentifierToken { get; }
        public string CloseComment => Tokens.CloseDoubleBrace;

        public ConstExpression(string value, string identifierToken) : this(new IdentifierExpression(value), identifierToken) {
        }

        public ConstExpression(LiteralExpression value, string identifierToken) {
            Value = value;
            IdentifierToken = identifierToken;
        }

        public override string Text {
            get {
                return Value.Text;
            }
        }
    }
}