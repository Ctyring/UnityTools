using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst {
    public class EqualsValueClauseList: LuaSyntaxNode {
        public string EqualsToken => LuaDefine.Tokens.Equals;
        public readonly LuaSyntaxList<Expression> Values = new();

        public EqualsValueClauseList() {

        }

        public EqualsValueClauseList(Expression value) {
            Values.Add(value);
        }

        public EqualsValueClauseList(IEnumerable<Expression> values) {
            Values.AddRange(values);
        }
    }
}