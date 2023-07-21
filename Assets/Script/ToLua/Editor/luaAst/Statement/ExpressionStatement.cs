namespace Script.ToLua.Editor.luaAst
{
    public class ExpressionStatement: Statement
    {
        public Expression _expression;
        
        public ExpressionStatement(Expression expression)
        {
            _expression = expression;
        }
        
        // 隐式转换
        public static implicit operator ExpressionStatement(Expression expression)
        {
            return new ExpressionStatement(expression);
        }
    }
}