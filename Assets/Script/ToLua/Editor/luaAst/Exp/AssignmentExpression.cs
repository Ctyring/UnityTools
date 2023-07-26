using System;

namespace Script.ToLua.Editor.luaAst {
    public class AssignmentExpression: Expression {
        public Expression Left { get; }
        public string OperatorToken => LuaDefine.Tokens.Equals;
        public Expression Right { get; }

        public AssignmentExpression(Expression left, Expression right) {
            Left = left ?? throw new ArgumentNullException(nameof(left));
            Right = right ?? throw new ArgumentNullException(nameof(right));
        }
    }
}