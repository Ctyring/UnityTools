using System;

namespace Script.ToLua.Editor.luaAst {
    public class ParenthesizedExpression: Expression {
        public Expression expression { get; }
        public string OpenParenToken => LuaDefine.Tokens.OpenParentheses;
        public string CloseParenToken => LuaDefine.Tokens.CloseParentheses;

        public ParenthesizedExpression(Expression expression) {
            this.expression = expression ?? throw new ArgumentNullException(nameof(expression));
        }
    }
}