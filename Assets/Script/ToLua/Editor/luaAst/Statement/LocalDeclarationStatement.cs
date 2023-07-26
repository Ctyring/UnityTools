using System;

namespace Script.ToLua.Editor.luaAst {
    public class LocalDeclarationStatement: Statement {
        public VariableDeclaration Declaration { get; }

        public LocalDeclarationStatement(VariableDeclaration declaration) {
            Declaration = declaration ?? throw new ArgumentNullException(nameof(declaration));
        }
    }
}