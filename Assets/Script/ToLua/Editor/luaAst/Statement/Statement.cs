namespace Script.ToLua.Editor.luaAst
{
    public abstract class Statement: LuaSyntaxNode {
        public bool ForceSemicolon = false;
        
        public static implicit operator Statement(Expression expression) {
            return new ExpressionStatement(expression);
        }
    }
}