namespace Script.ToLua.Editor.luaAst {
    public class MultipleAssignmentExpression: Expression {
        public LuaSyntaxList<Expression> Lefts { get; } = new();
        public string OperatorToken => LuaDefine.Tokens.Equals;
        public LuaSyntaxList<Expression> Rights { get; } = new();
    }
}