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
            List<Expression> codeExpressions = new();
            int preIdx = 0;
            // 遍历匹配的结果
            foreach (Match match in matches) {
                if (match.Index > preIdx) {
                    string preToken = codeTemplate[preIdx..match.Index];
                    if (!string.IsNullOrEmpty(preToken)) {
                        codeExpressions.Add(preToken);
                    }
                    
                    string comma = match.Groups[1].Value;
                    string key = match.Groups[2].Value;
                    switch (key) {
                        case "this":
                            var thisExpression = memberBindingIdentifierName ?? (target != null
                                ? BuildMemberAccessTargetExpression(target)
                                : LuaDefine.IdentifierName.This);
                            AddCodeTemplateExpression(thisExpression, comma, codeExpressions);
                            break;
                        case "class":
                            var type = _semanticModel.GetTypeInfo(target).Type;
                            var typeName = _generator.GetTypeName(type, this);
                            AddCodeTemplateExpression(typeName, comma, codeExpressions);
                    }
                }
            }
        }
        
        private void AddCodeTemplateExpression(Expression expression, string comma, List<Expression> codeExpression) {
            if (!string.IsNullOrEmpty(comma)) {
                codeExpression.Add(comma);
            }
            codeExpression.Add(expression);
        }
        
        /// <summary>
        /// 修正typename
        /// </summary>
        /// <param name="name"></param>
        /// <param name="symbol"></param>
        public void ImportTypeName(ref string name, INamedTypeSymbol symbol) {
            // 判断允许导出
            if (IsImportTypeNameEnable(symbol)) {
                int pos = name.LastIndexOf('.');
                if (pos != -1) {
                    string prefix = name[..pos];
                    if (prefix != LuaDefine.IdentifierName.System.name && prefix != LuaDefine.IdentifierName.Class.name) {
                        string newPrefix = prefix.Replace(".", "");
                        ProcessNewPrefix(ref newPrefix, prefix);
                        if (!IsLocalVarExistsInCurMethod(newPrefix)) {
                            bool success = AddImport(prefix, newPrefix, !symbol.DeclaringSyntaxReferences.IsEmpty);
                            if (success) {
                                name = newPrefix + name[pos..];
                            }
                        }
                    }
                }
            }
        }
        
        /// <summary>
        /// 加入到声明中
        /// </summary>
        /// <param name="prefix"></param>
        /// <param name="newPrefix"></param>
        /// <param name="isFromCode"></param>
        /// <returns></returns>
        private bool AddImport(string prefix, string newPrefix, bool isFromCode) {
            if (CurrentThunk.usingDeclares.Exists(i => i.Prefix == prefix && i.IsFromCode == isFromCode)) {
                return true;
            }

            CurrentThunk.usingDeclares.Add(new UsingDeclare {
                Prefix = prefix,
                NewPrefix = newPrefix,
                IsFromCode = isFromCode,
            });
            return true;
        }
        
        /// <summary>
        /// 处理前缀
        /// </summary>
        /// <param name="newPrefix"></param>
        /// <param name="prefix"></param>
        private void ProcessNewPrefix(ref string newPrefix, string prefix) {
            var usingDeclare = CurrentThunk.usingDeclares.Find(i => i.Prefix == prefix);
            // 如果已经被处理过了，就不用再处理了
            if (usingDeclare != null) {
                newPrefix = usingDeclare.NewPrefix;
                return;
            }

            string newName = newPrefix;
            const int kMaxNameLength = 25;
            // 如果长度超过25，就尝试去掉中间的部分
            if (newPrefix.Length > kMaxNameLength) {
                string[] names = prefix.Split('.');
                if (names.Length > 2) {
                    string head = names.First();
                    string tail = names.Last();
                    newName = head + tail;
                    if (newName.Length > kMaxNameLength) {
                        newName = tail;
                    }
                }
            }

            int index = 0;
            // 循环处理得到新的前缀
            while (true) {
                string result = CSharpToLuaSyntaxWalker.GetNewIdentifierName(newName, index);
                if (!CurrentThunk.usingDeclares.Exists(i => i.NewPrefix == result)) {
                    newPrefix = result;
                    return;
                }
                ++index;
            }
        }

        /// <summary>
        /// 判断类型名是否允许导出
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private bool IsImportTypeNameEnable(INamedTypeSymbol symbol) {
            // 除了泛型全部允许
            if (symbol.IsGenericType) {
                // 防止重复或者不属于当前方法
                if (IsTypeParameterExists(symbol) && !IsCurMethodTypeArgument(symbol)) {
                    return false;
                }
                return true;
            }
            return true;
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
                    // 判断是否存在参数类型
                    if (!IsTypeParameterExists(symbol)) {
                        // 没有参数，直接添加泛型导入
                        success = AddGenericImport(invocationExpression, newName, argumentTypeNames, IsAbsoluteFromCode(symbol));
                    } else {
                        success = CurTypeDeclaration.TypeDeclaration.AddGenericImport(invocationExpression, newName, argumentTypeNames, IsAbsoluteFromCode(symbol), out var declare);
                        // declare为null说明已经添加过
                        if (declare != null) {
                            // 添加泛型的依赖
                            bool hasAdd = _generator.AddGenericImportDepend(CurTypeDeclaration.TypeSymbol, symbol.OriginalDefinition as INamedTypeSymbol);
                            // 如果是声明冲突的，就添加到全局
                            if (hasAdd && CurrentThunk.IsUsingDeclareConflict(invocationExpression)) {
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
        /// 添加泛型引用声明
        /// </summary>
        /// <param name="invocationExpression"></param>
        /// <param name="name"></param>
        /// <param name="argumentTypeNames"></param>
        /// <param name="isFromCode"></param>
        /// <returns></returns>
        bool AddGenericImport(InvocationExpression invocationExpression, string name, List<string> argumentTypeNames, bool isFromCode) {
            if (CurrentThunk.genericUsingDeclares.Exists(i => i.NewName == name)) {
                return true;
            }
            
            CurrentThunk.genericUsingDeclares.Add(new GenericUsingDeclare {
                InvocationExpression = invocationExpression,
                ArgumentTypeNames = argumentTypeNames,
                NewName = name,
                IsFromCode = isFromCode
            });
            return true;
        }

        /// <summary>
        /// 判断是否定义在代码中
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public static bool IsAbsoluteFromCode(ITypeSymbol symbol) {
            if (!symbol.DeclaringSyntaxReferences.IsEmpty) {
                return true;
            }

            switch (symbol.Kind) {
                case SymbolKind.ArrayType: {
                    var arrayType = (IArrayTypeSymbol)symbol;
                    if (IsAbsoluteFromCode(arrayType.ElementType)) {
                        return true;
                    }
                    break;
                }
                case SymbolKind.NamedType: {
                    var nameTypeSymbol = (INamedTypeSymbol)symbol;
                    foreach (var typeArgument in nameTypeSymbol.TypeArguments) {
                        if (IsAbsoluteFromCode(typeArgument)) {
                            return true;
                        }
                    }
                    break;
                }
            }

            return false;
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
        
        /// <summary>
        /// 判断是否存在参数类型
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="matchType"></param>
        /// <returns></returns>
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