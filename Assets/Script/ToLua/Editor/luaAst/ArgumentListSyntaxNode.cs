using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst {
    public class ArgumentListSyntaxNode: LuaSyntaxNode {
        public string OpenParenToken => Tokens.OpenParentheses;
        public string CloseParenToken => Tokens.CloseParentheses;
        public readonly LuaSyntaxList<Expression> Arguments = new();
        public bool IsCallSingleTable { get; set; }

        public void AddArgument(Expression argument) {
            Arguments.Add(argument);
        }

        public void AddArguments(IEnumerable<Expression> arguments) {
            foreach (var argument in arguments) {
                AddArgument(argument);
            }
        }
    }
}