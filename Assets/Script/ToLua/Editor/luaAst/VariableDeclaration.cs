namespace Script.ToLua.Editor.luaAst {
    public abstract class VariableDeclaration : LuaSyntaxNode {
        public abstract bool IsEmpty { get; }

        public static implicit operator Statement(VariableDeclaration node) {
            return new LocalDeclarationStatement(node);
        }
    }
}