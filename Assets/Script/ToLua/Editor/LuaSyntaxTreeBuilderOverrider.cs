using System.Diagnostics.Contracts;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;
using ArgumentListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArgumentListSyntax;

namespace Script.ToLua.Editor {
    public partial class LuaSyntaxTreeBuilder {
        // 创建对象
        public override LuaSyntaxNode VisitObjectCreationExpression(ObjectCreationExpressionSyntax node) {
            var constExpression = GetConstExpression(node);
            if (constExpression != null) {
                return constExpression;
            }

            var symbol = (IMethodSymbol)ModelExtensions.GetSymbolInfo(_semanticModel, node).Symbol;
            Expression expression;
            if (symbol != null) {
                // 查找是否有对象创建的模板
                string codeTemplate = _generator.metaProvider.GetMethodCodeTemplate(symbol);
                if (codeTemplate != null) {
                    expression = BuildCodeTemplate(codeTemplate, null, symbol.TypeArguments,
                        FillCodeTemplateInvocationArguments(symbol, node.ArgumentList, null), node);
                }
                // 可空类型
                else if (node.Type.IsKind(SyntaxKind.NullableType)) {
                    Contract.Assert(node.ArgumentList!.Arguments.Count == 1);
                    var argument = node.ArgumentList.Arguments.First();
                    return argument.Expression.Accept(this);
                }
                // 元组类型
                else if (symbol.ContainingType.IsTupleType) {
                    var expressions = node.ArgumentList!.Arguments.Select(i => (Expression)i.Expression.Accept(this));
                    expression = BuildValueTupleCreateExpression(expressions);
                }
                // 对象类型
                else {
                    expression = GetObjectCreationExpression(symbol, node);
                }
            }
            else {
                // 判断是否是委托类型
                var type = _semanticModel.GetSymbolInfo(node.Type).Symbol;
                if (type?.Kind == SymbolKind.NamedType) {
                    var nameType = (INamedTypeSymbol)type;
                    if (nameType.TypeKind == TypeKind.Delegate) {
                        Contract.Assert(node.ArgumentList!.Arguments.Count == 1);
                        var argument = node.ArgumentList.Arguments.First();
                        return argument.Expression.Accept(this);
                    }
                }

                Contract.Assert(!node.ArgumentList!.Arguments.Any());
                var acptexpression = (Expression)node.Type.Accept(this);
                expression = new InvocationExpression(acptexpression);
            }

            // 初始化对象创建
            return GetObjectCreationInitializer(expression, node);
        }

        
    }
}