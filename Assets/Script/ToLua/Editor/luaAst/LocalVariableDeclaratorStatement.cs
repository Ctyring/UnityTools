namespace Script.ToLua.Editor.luaAst
{
    public class LocalVariableDeclaratorStatement: Statement
    {
        IdentifierName _identifierName; // 标识符
        private EqualsValueClause _clause; // 赋值语句
        
        public LocalVariableDeclaratorStatement(IdentifierName name, Expression expression)
        {
            _identifierName = name;
            _clause = new EqualsValueClause(expression);
        }
    }
}