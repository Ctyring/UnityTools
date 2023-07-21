namespace Script.ToLua.Editor.luaAst
{
    public class LocalVariableDeclaratorStatement: Statement
    {
        IdentifierNameExpression _identifierNameExpression; // 标识符
        private EqualsValueClause _clause; // 赋值语句
        
        public LocalVariableDeclaratorStatement(IdentifierNameExpression nameExpression, Expression expression)
        {
            _identifierNameExpression = nameExpression;
            _clause = new EqualsValueClause(expression);
        }
    }
}