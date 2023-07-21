namespace Script.ToLua.Editor.luaAst
{
    public abstract class Statement: LuaSyntaxNode
    {
        public static implicit operator Statement(Expression expression) {
            return new ExpressionStatement(expression);
        }
    }
}