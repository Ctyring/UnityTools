using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.CSharpToLuaHelper;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor {
    public partial class LuaSyntaxTreeBuilder {
        private static readonly Regex codeTemplateRegex_ = new(@"(,?\s*)\{(\*?[\w|`]+)\}", RegexOptions.Compiled); // 匹配 {xxx} 或 {xxx*} 或 {,xxx} 或 {,xxx*}
        private int noImportTypeNameCounter_;
        public bool IsNoImportTypeName => noImportTypeNameCounter_ > 0;
        /// <summary>
        /// 构建代码模板
        /// </summary>
        /// <returns></returns>
        private Exception BuildCodeTemplate(string codeTemplate, IdentifierNameExpression memberBindingIdentifierName, ExpressionSyntax target) {
            MatchCollection matches = codeTemplateRegex_.Matches(codeTemplate);
            List<Expression> codeExceptions = new();
            int preIdx = 0;
            // 遍历匹配的结果
            foreach (Match match in matches) {
                if (match.Index > preIdx) {
                    string preToken = codeTemplate[preIdx..match.Index];
                    if (!string.IsNullOrEmpty(preToken)) {
                        codeExceptions.Add(preToken);
                    }
                    
                    string comma = match.Groups[1].Value;
                    string key = match.Groups[2].Value;
                    switch (key) {
                        case "this":
                            var thisExpression = memberBindingIdentifierName ?? (target != null
                                ? BuildMemberAccessTargetExpression(target)
                                : LuaDefine.IdentifierName.This);
                            if (!string.IsNullOrEmpty(comma)) {
                                codeExceptions.Add(comma);
                            }
                            codeExceptions.Add(thisExpression);
                            break;
                        case "class":
                            var type = _semanticModel.GetTypeInfo(target).Type;
                            var typeName = _generator.GetType()
                    }
                }
            }
        }
        
        /// <summary>
        /// 导入泛型类型名
        /// </summary>
        /// <param name="luaExpression"></param>
        /// <param name="symbol"></param>
        public void ImportGenericTypeName(ref Expression luaExpression, ITypeSymbol symbol) {
            if (!IsNoImportTypeName && !SymbolEqualityComparer.Default.Equals(CurTypeSymbol, symbol) && !IsCurMethodTypeArgument(symbol)) {
                var invocationExpression = (InvocationExpression)luaExpression;
                string newName = GetGenericTypeImportName(invocationExpression, out var argumentTypeNames);
                // 判断是否是本地变量
                if (!IsLocalVarExistsInCurMethod(newName)) {
                    bool success;
                    if (!symbol.IsTypeParameterExists()) {
                        success = AddGenericImport(invocationExpression, newName, argumentTypeNames, symbol.IsAbsoluteFromCode());
                    } else {
                        success = CurTypeDeclaration.TypeDeclaration.AddGenericImport(invocationExpression, newName, argumentTypeNames, symbol.IsAbsoluteFromCode(), out var declare);
                        if (declare != null) {
                            bool hasAdd = generator_.AddGenericImportDepend(CurTypeDeclaration.TypeSymbol, symbol.OriginalDefinition as INamedTypeSymbol);
                            if (hasAdd && CurCompilationUnit.IsUsingDeclareConflict(invocationExpression)) {
                                declare.IsFromGlobal = true;
                                CurTypeDeclaration.TypeDeclaration.AddGlobalParameter();
                            }
                        }
                    }
                    if (success) {
                        luaExpression = newName;
                    }
                }
            }
        }
        
        /// <summary>
        /// 判断是否是当前方法的本地变量
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        private bool IsLocalVarExistsInCurMethod(string name) {
            var methodInfo = CurMethodInfoOrNull;
            if (methodInfo != null) {
                var root = GetDeclaringSyntaxNode(methodInfo.Symbol);
                if (IsLocalVarExists(name, root)) {
                    return true;
                }
            }
            return false;
        }
        
        private static bool IsLocalVarExists(string name, SyntaxNode root) {
            var searcher = new LocalValueWalker(name);
            return searcher.Find(root);
        }
        
        public static SyntaxNode GetDeclaringSyntaxNode(ISymbol symbol) {
            return symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
        }
        
        private static string GetGenericTypeImportName(InvocationExpression invocationExpression, out List<string> argumentTypeNames) {
            StringBuilder sb = new StringBuilder();
            argumentTypeNames = new List<string>();
            FillGenericTypeImportName(sb, argumentTypeNames, invocationExpression);
            return sb.ToString();
        }
        
        static string CheckLastName(string lastName) {
            return lastName == "Dictionary" ? "Dict" : lastName;
        }
        
        /// <summary>
        /// 获取需要填充泛型的类型名
        /// </summary>
        /// <param name="sb"></param>
        /// <param name="argumentTypeNames"></param>
        /// <param name="invocationExpression"></param>
        private static void FillGenericTypeImportName(StringBuilder sb, List<string> argumentTypeNames, InvocationExpression invocationExpression) {
            var identifierName = (IdentifierNameExpression)invocationExpression.expression;
            sb.Append(CheckLastName(LastName(identifierName.name)));
            foreach (var argument in invocationExpression.arguments) {
                if (argument is IdentifierNameExpression typeName) {
                    string argumentTypeName = typeName.name;
                    sb.Append(CheckLastName(LastName(argumentTypeName)));
                    argumentTypeNames.Add(argumentTypeName);
                } else {
                    FillGenericTypeImportName(sb, argumentTypeNames, (InvocationExpression)argument);
                }
            }
        }
        
        public static string LastName(string s) {
            int pos = s.LastIndexOf('.');
            if (pos != -1) {
                return s[(pos + 1)..];
            }
            return s;
        }
        
        private bool IsCurMethodTypeArgument(ITypeSymbol symbol) {
            var methodInfo = CurMethodInfoOrNull;
            if (methodInfo != null) {
                return IsMethodTypeArgument(methodInfo.Symbol, symbol);
            }
            return false;
        }
        
        private static bool IsMethodTypeArgument(IMethodSymbol method, ITypeSymbol symbol) {
            if (method.TypeArguments.Length > 0) {
                return method.TypeArguments.Any(i=>IsTypeParameterExists(i));
            }

            if (method.MethodKind is MethodKind.LambdaMethod or MethodKind.LocalFunction) {
                if (method.ContainingSymbol is IMethodSymbol containingMethod) {
                    return IsMethodTypeArgument(containingMethod, symbol);
                }
            }
            return false;
        }
        
        public static bool IsTypeParameterExists(ITypeSymbol symbol, ITypeSymbol matchType = null) {
            switch (symbol.Kind) {
                case SymbolKind.ArrayType: {
                    var arrayType = (IArrayTypeSymbol)symbol;
                    if (IsTypeParameterExists(arrayType.ElementType, matchType)) {
                        return true;
                    }
                    break;
                }
                case SymbolKind.NamedType: {
                    var nameTypeSymbol = (INamedTypeSymbol)symbol;
                    foreach (var typeArgument in nameTypeSymbol.TypeArguments) {
                        if (IsTypeParameterExists(typeArgument, matchType)) {
                            return true;
                        }
                    }
                    if (symbol.ContainingType != null) {
                        if (IsTypeParameterExists(symbol.ContainingType, matchType)) {
                            return true;
                        }
                    }
                    break;
                }
                case SymbolKind.TypeParameter: {
                    return matchType == null || SymbolEqualityComparer.Default.Equals(symbol, matchType);
                }
                case SymbolKind.PointerType: {
                    var pointType = (IPointerTypeSymbol)symbol;
                    if (IsTypeParameterExists(pointType.PointedAtType, matchType)) {
                        return true;
                    }
                    break;
                }
            }

            return false;
        }

        private Expression BuildMemberAccessTargetExpression(ExpressionSyntax targetExpression) {
            var expression = (Expression)targetExpression.Accept(this);
            SyntaxKind kind = targetExpression.Kind();
            if ((kind >= SyntaxKind.NumericLiteralExpression && kind <= SyntaxKind.NullLiteralExpression) ||
                (expression is ValExpression)) {
                ProcessPrevIsInvokeStatement(targetExpression);
                expression = expression.Parenthesized();
            }

            return expression;
        }
        
        private void ProcessPrevIsInvokeStatement(ExpressionSyntax node) {
            SyntaxNode current = node;
            while (true) {
                var parent = current.Parent;
                if (parent == null) {
                    return;
                }

                switch (parent.Kind()) {
                    case SyntaxKind.Argument:
                    case SyntaxKind.LocalDeclarationStatement:
                    case SyntaxKind.CastExpression: {
                        return;
                    }

                    default: {
                        if (parent is AssignmentExpressionSyntax assignment && assignment.Right == current) {
                            return;
                        }
                        break;
                    }
                }

                if (parent.IsKind(SyntaxKind.ExpressionStatement)) {
                    break;
                }
                current = parent;
            }

            var curBlock = _blocks.Count > 0 ? _blocks.Peek() : null;
            if (curBlock != null) {
                var statement = curBlock.statements.FindLast(i => i is not BlankLineStatement && i is not CommentStatement);
                if (statement != null) {
                    statement.ForceSemicolon = true;
                }
            }
        }

        public ValExpression GetConstExpression(ExpressionSyntax node) {
            var constValue = _semanticModel.GetConstantValue(node);
            if (constValue.HasValue) {
                switch (constValue.Value) {
                    case double d:
                        switch (d) {
                            case double.NegativeInfinity:
                            case double.PositiveInfinity:
                            case double.NaN:
                                return null;
                        }
                        break;
                    case float f:
                        switch (f) {
                            case float.NegativeInfinity:
                            case float.PositiveInfinity:
                            case float.NaN:
                                return null;
                        }
                        break;
                    case null:
                        return new ConstExpression(LuaDefine.IdentifierName.Nil.name, "nil");
                }
                
                ValExpression expression = GetLiteralExpression(constValue.Value);
                return new ConstExpression(expression.value, node.ToString());
            }
            return null;
        }
        
        private ValExpression GetLiteralExpression(object constantValue) {
            if (constantValue != null) {
                var code = Type.GetTypeCode(constantValue.GetType());
                switch (code) {
                    case TypeCode.Char: {
                        Debug.Assert(constantValue != null, nameof(constantValue) + " != null");
                        return new CharExpression(((int)constantValue).ToString(), SyntaxFactory.Literal((char)constantValue).Text);
                    }
                    case TypeCode.String: {
                        return BuildStringLiteralExpression((string)constantValue);
                    }
                    case TypeCode.Boolean: {
                        bool v = (bool)constantValue;
                        return v ? new ValExpression(LuaDefine.IdentifierName.True.name, LuaDefine.IdentifierName.True) : new ValExpression(LuaDefine.IdentifierName.False.name, LuaDefine.IdentifierName.False);
                    }
                    case TypeCode.Single: {
                        float v = (float)constantValue;
                        return new FloatExpression(v);
                    }
                    case TypeCode.Double: {
                        double v = (double)constantValue;
                        return new DoubleExpression(v);
                    }
                    case TypeCode.Int64: {
                        if (constantValue is long.MinValue) {
                            const long kMinInteger = long.MinValue + 1;     // in lua5.4 long.MinValue will be float
                            return new IdentifierExpression($"({kMinInteger} - 1)");
                        }
                        break;
                    }
                }
                return new IdentifierExpression(constantValue.ToString());
            }

            return new ValExpression(LuaDefine.IdentifierName.Nil.name, LuaDefine.IdentifierName.Nil);
        }
        
        private StringExpression BuildStringLiteralExpression(string value) {
            string text = SyntaxFactory.Literal(value).Text;
            return new StringExpression(text);
        }
    }
}