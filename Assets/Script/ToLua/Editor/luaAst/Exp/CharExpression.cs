using Microsoft.CodeAnalysis.CSharp;

namespace Script.ToLua.Editor.luaAst {
    public class CharExpression: ConstExpression {
        
        public CharExpression(char character) : base(((int)character).ToString(), GetIdentifierToken(character)) {
        }

        private static string GetIdentifierToken(char character) {
            return SyntaxFactory.Literal(character).Text;
        }
    }
}