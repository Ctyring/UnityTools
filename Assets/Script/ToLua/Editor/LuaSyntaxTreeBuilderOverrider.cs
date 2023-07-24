using System.Diagnostics.Contracts;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
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
            } else {
                var type = _semanticModel.GetSymbolInfo(node.Type).Symbol;
                if (type?.Kind == SymbolKind.NamedType) {
                    var nameType = (INamedTypeSymbol)type;
                    if (nameType.IsDelegateType()) {
                        Contract.Assert(node.ArgumentList!.Arguments.Count == 1);
                        var argument = node.ArgumentList.Arguments.First();
                        return argument.Expression.Accept(this);
                    }
                }

                Contract.Assert(!node.ArgumentList!.Arguments.Any());
                var acptexpression = (ObjectCreationExpressionSyntax)node.Type.Accept(this);
                expression = new InvocationExpression(acptexpression);
            }

            return GetObjectCreationInitializer(expression, node);
        }
    }
}