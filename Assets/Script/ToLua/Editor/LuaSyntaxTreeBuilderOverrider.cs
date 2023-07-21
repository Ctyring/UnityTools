using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor {
    public partial class LuaSyntaxTreeBuilder {
        // 创建对象
        public override LuaSyntaxNode VisitObjectCreationExpression(ObjectCreationExpressionSyntax node) {
            var constExpression = GetConstExpression(node);
            if (constExpression != null) {
                return constExpression;
            }

            var symbol = (IMethodSymbol)_semanticModel.GetSymbolInfo(node).Symbol;
            Expression expression;
            if (symbol != null) {
                
            }
        }
    }
}