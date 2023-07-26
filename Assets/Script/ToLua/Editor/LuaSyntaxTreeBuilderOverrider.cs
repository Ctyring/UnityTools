using System.Diagnostics.Contracts;
using System.Linq;
using CSharpLua;
using CSharpLua.LuaAst;
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
        
        /// <summary>
        /// 匿名对象声明 new { Name = "John", Age = 25 }
        /// </summary>
        /// <param name="node"></param>
        /// <returns>Lua中table的键值对</returns>
        public override LuaSyntaxNode VisitAnonymousObjectMemberDeclarator(AnonymousObjectMemberDeclaratorSyntax node) {
            IdentifierNameExpression name;
            var expression = (Expression)node.Expression.Accept(this);
            if (node.NameEquals != null) {
                name = (IdentifierNameExpression)node.NameEquals.Accept(this);
            } else {
                var property = _semanticModel.GetDeclaredSymbol(node);
                name = property!.Name;
            }
            return new LuaKeyValueTableItemSyntax(name, expression);
        }
        
        /// <summary>
        /// 匿名对象创建 var obj = new {Name = "Hello", Age = 18};
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        public override LuaSyntaxNode VisitAnonymousObjectCreationExpression(AnonymousObjectCreationExpressionSyntax node) {
            var table = new LuaTableExpression();
            // 对象的成员构建
            foreach (var initializer in node.Initializers) {
                var item = (LuaKeyValueTableItemSyntax)initializer.Accept(this);
                table.Items.Add(item);
            }
            return LuaDefine.IdentifierName.AnonymousType.Invocation(table);
        }
        
        /// <summary>
        /// 隐式对象创建 var num = new[] {1, 2, 3};
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        public override LuaSyntaxNode VisitImplicitObjectCreationExpression(ImplicitObjectCreationExpressionSyntax node) {
            var symbol = (IMethodSymbol)_semanticModel.GetSymbolInfo(node).Symbol;
            string codeTemplate = _generator.metaProvider.GetMethodCodeTemplate(symbol);
            var creationExpression = codeTemplate != null 
                ? BuildCodeTemplate(codeTemplate, null, symbol.TypeArguments, FillCodeTemplateInvocationArguments(symbol, node.ArgumentList, null), node)
                : GetObjectCreationExpression(symbol, node);
            return GetObjectCreationInitializer(creationExpression, node);
        }
        
        /// <summary>
        /// 变量/对象初始化表达式 int[] num = {1, 2, 3};
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        public override LuaSyntaxNode VisitInitializerExpression(InitializerExpressionSyntax node) {
            if (node.IsKind(SyntaxKind.ArrayInitializerExpression)) {
                var arrayType = (IArrayTypeSymbol)_semanticModel.GetTypeInfo(node).ConvertedType;
                return BuildArrayTypeFromInitializer(arrayType, node);
            }

            var type = _semanticModel.GetTypeInfo(node!.Parent!).Type;
            var expression = _generator.GetTypeName(type);
            return expression.Invocation();
        }
    }
}