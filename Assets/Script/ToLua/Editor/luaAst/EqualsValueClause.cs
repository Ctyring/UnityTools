namespace Script.ToLua.Editor.luaAst
{
    public class EqualsValueClause: LuaSyntaxNode
    {
        private Expression _value;
        public EqualsValueClause(Expression expression)
        {
            _value = expression;
        }
    }
}