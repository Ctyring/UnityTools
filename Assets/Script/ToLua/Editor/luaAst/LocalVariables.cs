using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst {
    public class LocalVariables: VariableDeclaration {
        public string LocalKeyword => LuaDefine.Keyword.Local;
        public readonly LuaSyntaxList<IdentifierNameExpression> Variables = new();
        public EqualsValueClauseList Initializer { get; set; }

        public LocalVariables() {
        }

        public LocalVariables(IEnumerable<IdentifierNameExpression> variables, IEnumerable<Expression> values = null) {
            Variables.AddRange(variables);
            if (values != null) {
                Initializer = new EqualsValueClauseList(values);
            }
        }

        public override bool IsEmpty => Variables.Count == 0;
    }
}