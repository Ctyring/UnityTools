using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.Contracts;
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
        private static readonly Regex
            codeTemplateRegex_ =
                new(@"(,?\s*)\{(\*?[\w|`]+)\}", RegexOptions.Compiled); // 匹配 {xxx} 或 {xxx*} 或 {,xxx} 或 {,xxx*}

        private int noImportTypeNameCounter_;
        public bool IsNoImportTypeName => noImportTypeNameCounter_ > 0;

        /// <summary>
        /// 构建代码模板
        /// </summary>
        /// <returns></returns>
        private ListExpression BuildCodeTemplate(string codeTemplate, IdentifierNameExpression memberBindingIdentifierName,
            IList<ITypeSymbol> typeArguments,IEnumerable<Func<Expression>> arguments,
            ExpressionSyntax target) {
            MatchCollection matches = codeTemplateRegex_.Matches(codeTemplate);
            ListExpression codeExpressions = new();
            int preIdx = 0;
            // 遍历匹配的结果
            foreach (Match match in matches) {
                if (match.Index > preIdx) {
                    string preToken = codeTemplate[preIdx..match.Index];
                    if (!string.IsNullOrEmpty(preToken)) {
                        codeExpressions.list.Add(preToken);
                    }

                    string comma = match.Groups[1].Value;
                    string key = match.Groups[2].Value;
                    switch (key) {
                        case "this":
                            var thisExpression = memberBindingIdentifierName ?? (target != null
                                ? BuildMemberAccessTargetExpression(target)
                                : LuaDefine.IdentifierName.This);
                            AddCodeTemplateExpression(thisExpression, comma, codeExpressions.list);
                            break;
                        case "class":
                            var type = _semanticModel.GetTypeInfo(target).Type;
                            var typeNameExp = _generator.GetTypeName(type, this);
                            AddCodeTemplateExpression(typeNameExp, comma, codeExpressions.list);
                            break;
                        default: {
                            switch (key[0]) {
                                case '`': {
                                    // ``情况
                                    if (key.StartsWith("``")) {
                                        if (int.TryParse(key[2..], out int classTypeIndex)) {
                                            // 获取class
                                            var classType =
                                                (INamedTypeSymbol)_semanticModel.GetTypeInfo(target).Type;
                                            ITypeSymbol typeArgument = default;
                                            // 参数从class的参数列表中取，即``标记了使用class第几个arg
                                            if (classType.TypeArguments.Length > classTypeIndex) {
                                                typeArgument = classType.TypeArguments[classTypeIndex];
                                            }
                                            if (typeArgument != null) {
                                                var typeName = _generator.GetTypeName(typeArgument);
                                                AddCodeTemplateExpression(typeName, comma, codeExpressions.list);
                                            }
                                        }
                                    }
                                    // `情况
                                    else if (int.TryParse(key[1..], out int typeIndex)) {
                                        // 从typelist中拿到对应的type
                                        ITypeSymbol typeArgument = default;
                                        if (typeArguments.Count > typeIndex) {
                                            typeArgument = typeArguments[typeIndex];
                                        }
                                        if (typeArgument != null) {
                                            Expression typeName;
                                            // 如果是enum，需要导出
                                            if (typeArgument.TypeKind == TypeKind.Enum &&
                                                codeTemplate.StartsWith("System.Enum")) {
                                                typeName = _generator.GetTypeShortName(typeArgument);
                                                _generator.AddExportEnum(typeArgument);
                                            }
                                            else {
                                                typeName = _generator.GetTypeName(typeArgument);
                                            }

                                            AddCodeTemplateExpression(typeName, comma, codeExpressions.list);
                                        }
                                    }

                                    break;
                                }
                                case '*': {
                                    // 从paramsIndex开始，取出所有参数
                                    if (int.TryParse(key[1..], out int paramsIndex)) {
                                        List<Expression> expressions = new List<Expression>();
                                        foreach (var argument in arguments.Skip(paramsIndex)) {
                                            var argumentExpression = argument();
                                            expressions.Add(argumentExpression);
                                        }

                                        if (expressions.Count > 0) {
                                            // AddCodeTemplateExpression(expressions, comma, codeExpressions.list);
                                            foreach (Expression expression in expressions) {
                                                AddCodeTemplateExpression(expression, comma, codeExpressions.list);
                                            }
                                        }
                                    }
                                    break;
                                }
                                default: {
                                    // 取出对应参数
                                    if (int.TryParse(key, out int argumentIndex)) {
                                        var argument = arguments?.ElementAtOrDefault(argumentIndex);
                                        if (argument != null) {
                                            var argumentExpression = argument();
                                            AddCodeTemplateExpression(argumentExpression, comma,
                                                codeExpressions.list);
                                        }
                                    }

                                    break;
                                }
                            }
                            break;
                        }
                    }
                }
                preIdx = match.Index + match.Length;
            }
            // 剩余的部分
            if (preIdx < codeTemplate.Length) {
                string last = codeTemplate[preIdx..];
                codeExpressions.list.Add(last);
            }

            return codeExpressions;
        }
        
        /// <summary>
        /// 填充使用模板生成代码必须的参数
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="argumentList"></param>
        /// <param name="argumentExpressions">填充后的参数列表</param>
        /// <returns>填充后的参数列表</returns>
        /// <exception cref="InvalidOperationException"></exception>
        private List<Func<Expression>> FillCodeTemplateInvocationArguments(IMethodSymbol symbol, ArgumentListSyntax argumentList, List<Func<Expression>> argumentExpressions) {
            argumentExpressions ??= new List<Func<Expression>>();
            foreach (var argument in argumentList.Arguments) {
                // 判断是否使用了冒号
                if (argument.NameColon != null) {
                    string name = argument.NameColon.Name.Identifier.ValueText;
                    int index = -1;
                    // 根据名字找到对应的参数
                    foreach (IParameterSymbol parameter in symbol.Parameters) {
                        if (parameter.Name == name) {
                            index = symbol.Parameters.IndexOf(parameter);
                            break;
                        }
                    }
                    // 没找到
                    if (index == -1) {
                        throw new InvalidOperationException();
                    }
                    
                    // argumentExpressions.AddAt(index, () => );
                    if (index < argumentExpressions.Count) {
                        // 填充对应下标的参数
                        argumentExpressions[index] = () => VisitExpression(argument.Expression);
                    } else {
                        // 扩容
                        int count = index - argumentExpressions.Count;
                        for (int i = 0; i < count; ++i) {
                            argumentExpressions.Add(default);
                        }
                        argumentExpressions.Add(() => VisitExpression(argument.Expression));
                    }
                } else {
                    // 正常的参数，就直接遍历处理
                    argumentExpressions.Add(() => VisitExpression(argument.Expression));
                }
            }

            // 填充默认参数
            for (int i = 0; i < argumentExpressions.Count; ++i) {
                argumentExpressions[i] ??= () => GetDefaultParameterValue(symbol.Parameters[i], argumentList.Parent, true);
            }

            // 在末尾填充默认参数
            if (symbol.Parameters.Length > argumentList.Arguments.Count) {
                argumentExpressions.AddRange(symbol.Parameters.Skip(argumentList.Arguments.Count).Where(i => !i.IsParams).Select(i => {
                    Expression Func() => GetDefaultParameterValueExpression(i, argumentList, true);
                    return (Func<Expression>)Func;
                }));
            }

            return argumentExpressions;
        }
        
        Expression GetDefaultParameterValueExpression(IParameterSymbol i, ArgumentListSyntax argumentList, bool isCheckCallerAttribute)
        {
            return GetDefaultParameterValue(i, argumentList.Parent, true);
        }
        
        private Expression GetDefaultParameterValue(IParameterSymbol parameter, SyntaxNode node, bool isCheckCallerAttribute) {
            Contract.Assert(parameter.HasExplicitDefaultValue);
            Expression defaultValue = isCheckCallerAttribute ? CheckCallerAttribute(parameter, node) : null;
            if (defaultValue == null) {
                if (parameter.ExplicitDefaultValue == null && parameter.Type.IsValueType) {
                    defaultValue = GetDefaultValueExpression(parameter.Type);
                } else {
                    defaultValue = GetLiteralExpression(parameter.ExplicitDefaultValue);
                }
            }
            Contract.Assert(defaultValue != null);
            return defaultValue;
        }
        
        private Expression GetDefaultValueExpression(ITypeSymbol typeSymbol) {
            if (typeSymbol.IsReferenceType) {
                return IdentifierExpression.Nil;
            }

            if (typeSymbol.IsValueType) {
                if (IsNullableType(typeSymbol)) {
                    return IdentifierExpression.Nil;
                }

                if (typeSymbol.IsTupleType) {
                    return GetValueTupleDefaultExpression(typeSymbol);
                }

                var predefinedValueType = GetPredefinedValueTypeDefaultValue(typeSymbol);
                if (predefinedValueType != null) {
                    return predefinedValueType;
                }
            }

            var typeName = _generator.GetTypeName(typeSymbol);
            return BuildDefaultValue(typeName);
        }
        
        /// <summary>
        /// 默认值调用表达式
        /// </summary>
        /// <param name="typeExpression"></param>
        /// <returns></returns>
        private static InvocationExpression BuildDefaultValue(Expression typeExpression) {
            return new(LuaDefine.IdentifierName.SystemDefault, typeExpression);
        }
        
        private Expression GetPredefinedValueTypeDefaultValue(ITypeSymbol typeSymbol) {
            switch (typeSymbol.SpecialType) {
                case SpecialType.None: {
                    if (typeSymbol.TypeKind == TypeKind.Enum) {
                        if (!_generator.IsConstantEnum(typeSymbol)) {
                            return BuildEnumNoConstantDefaultValue(typeSymbol);
                        }
                        return ValExpression.Zero;
                    }

                    if (typeSymbol.IsTimeSpanType()) {
                        return BuildDefaultValue(LuaDefine.IdentifierName.TimeSpan);
                    }
                    return null;
                }
                case SpecialType.System_Boolean: {
                    return new LuaIdentifierLiteralExpressionSyntax(LuaDefine.IdentifierName.False);
                }
                case SpecialType.System_Char: {
                    return new LuaCharacterLiteralExpression(default);
                }
                case SpecialType.System_SByte:
                case SpecialType.System_Byte:
                case SpecialType.System_Int16:
                case SpecialType.System_UInt16:
                case SpecialType.System_Int32:
                case SpecialType.System_UInt32:
                case SpecialType.System_Int64:
                case SpecialType.System_UInt64: {
                    return ValExpression.Zero;
                }
                case SpecialType.System_Single:
                case SpecialType.System_Double: {
                    return ValExpression.ZeroFloat;
                }
                case SpecialType.System_DateTime: {
                    return BuildDefaultValue(LuaDefine.IdentifierName.DateTime);
                }
                default:
                    return null;
            }
        }
        
        private Expression BuildEnumNoConstantDefaultValue(ITypeSymbol typeSymbol) {
            var typeName = _generator.GetTypeName(typeSymbol);
            var field = typeSymbol.GetMembers().OfType<IFieldSymbol>().FirstOrDefault(i => i.ConstantValue.Equals(0));
            if (field != null) {
                return typeName.MemberAccess(field.Name);
            }
            return typeName.Invocation(ValExpression.Zero);
        }
        
        private Expression GetValueTupleDefaultExpression(ITypeSymbol typeSymbol) {
            var elementTypes = GetTupleElementTypes(typeSymbol);
            return BuildValueTupleCreateExpression(elementTypes.Select(GetDefaultValueExpression));
        }
        
        /// <summary>
        /// 构造对象创建表达式
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="node"></param>
        /// <returns></returns>
        private Expression GetObjectCreationExpression(IMethodSymbol symbol, BaseObjectCreationExpressionSyntax node) {
            Expression creationExpression;
            var expression = _generator.GetTypeName(symbol.ContainingType);
            var invokeExpression = BuildObjectCreationInvocation(symbol, expression);
            if (node.ArgumentList != null) {
                var refOrOutArguments = new List<RefOrOutArgument>();
                var arguments = BuildArgumentList(symbol, symbol.Parameters, node.ArgumentList, refOrOutArguments);
                TryRemoveNilArgumentsAtTail(symbol, arguments);
                invokeExpression.AddArguments(arguments);
                creationExpression = refOrOutArguments.Count > 0
                    ? BuildInvokeRefOrOut(node, invokeExpression, refOrOutArguments)
                    : invokeExpression;
            } else {
                creationExpression = invokeExpression;
            }
            return creationExpression;
        }
        
        private LuaInvocationExpressionSyntax BuildObjectCreationInvocation(IMethodSymbol symbol, LuaExpressionSyntax expression) {
            int constructorIndex = GetConstructorIndex(symbol);
            if (constructorIndex > 1) {
                return new LuaInvocationExpressionSyntax(LuaIdentifierNameSyntax.SystemNew, expression, constructorIndex.ToString());
            }
            return new LuaInvocationExpressionSyntax(expression);
        }
        
        /// <summary>
        /// 构建值元组表达式
        /// </summary>
        /// <param name="expressions"></param>
        /// <returns></returns>
        private Expression BuildValueTupleCreateExpression(IEnumerable<Expression> expressions) {
            return LuaDefine.IdentifierName.ValueTuple.Invocation(expressions);
        }
        
        public static IEnumerable<ITypeSymbol> GetTupleElementTypes(ITypeSymbol typeSymbol) {
            var nameSymbol = (INamedTypeSymbol)typeSymbol;
            return nameSymbol.TupleElements.Select(i => i.Type);
        }
        
        public static bool IsNullableType(ITypeSymbol type) {
            return type.OriginalDefinition.SpecialType == SpecialType.System_Nullable_T;
        }

        public static bool IsNullableType(ITypeSymbol type, out ITypeSymbol elementType) {
            elementType = NullableElementType(type);
            return elementType != null;
        }
        
        public static ITypeSymbol NullableElementType(ITypeSymbol type) {
            return IsNullableType(type) ? ((INamedTypeSymbol)type).TypeArguments.First() : null;
        }
        
        private enum CallerAttributeKind {
            None,
            Line,
            Member,
            FilePath,
            ArgumentExpression,
        }
        
        private Expression CheckCallerAttribute(IParameterSymbol parameter, SyntaxNode node) {
            var kind = GetCallerAttributeKind(parameter);
            switch (kind) {
                case CallerAttributeKind.Line: {
                    var lineSpan = node.SyntaxTree.GetLineSpan(node.Span);
                    return lineSpan.StartLinePosition.Line + 1;
                }
                case CallerAttributeKind.Member: {
                    string memberName = null;
                    FindParent(node, i => {
                        switch (i.Kind()) {
                            case SyntaxKind.MethodDeclaration: {
                                var method = (MethodDeclarationSyntax)i;
                                memberName = method.Identifier.ValueText;
                                return true;
                            }
                            case SyntaxKind.PropertyDeclaration: {
                                var property = (PropertyDeclarationSyntax)i;
                                memberName = property.Identifier.ValueText;
                                return true;
                            }
                            case SyntaxKind.EventDeclaration: {
                                var @event = (EventDeclarationSyntax)i;
                                memberName = @event.Identifier.ValueText;
                                return true;
                            }
                        }
                        return false;
                    });
                    Contract.Assert(memberName != null);
                    return new LuaStringLiteralExpressionSyntax(memberName);
                }
                case CallerAttributeKind.FilePath: {
                    return BuildStringLiteralExpression(_generator.RemoveBaseFolder(node.SyntaxTree.FilePath));
                }
                case CallerAttributeKind.ArgumentExpression: {
                    var invocation = (InvocationExpressionSyntax)node;
                    var text = invocation.ArgumentList.Arguments.FirstOrDefault()?.ToString() ?? string.Empty;
                    return new StringExpression(text);
                }
                default:
                    return null;
            }
        }
        
        /// <summary>
        /// 遍历node
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        private Expression VisitExpression(ExpressionSyntax node) {
            // 遍历node
            var exp = (Expression)node.Accept(this);
            // 处理类型转换
            CheckConversion(node, ref exp);
            return exp;
        }
        
        /// <summary>
        /// 处理类型转换
        /// </summary>
        /// <param name="node"></param>
        /// <param name="expression"></param>
        private void CheckConversion(ExpressionSyntax node, ref Expression expression) {
            if (IsUserConversion(_semanticModel, node, out var methodSymbol)) {
                // 构建转换方法
                expression = BuildConversionExpression(methodSymbol, expression);
            }
        }
        
        /// <summary>
        /// 构建转换方法
        /// </summary>
        /// <param name="methodSymbol"></param>
        /// <param name="expression"></param>
        /// <returns></returns>
        private Expression BuildConversionExpression(IMethodSymbol methodSymbol, Expression expression) {
            // 获取转换方法的模板
            var codeTemplate = _generator.metaProvider.GetMethodCodeTemplate(methodSymbol);
            if (codeTemplate != null) {
                var args = new [] { expression };
                // 构建
                return BuildCodeTemplate(codeTemplate, null, null, args.Select<Expression, Func<Expression>>(i => () => i), null);
            }
            
            // 如果没有模板，构建成员访问的方法
            var memberAccess = GetOperatorMemberAccessExpression(methodSymbol);
            return new InvocationExpression(memberAccess, expression);
        }
        
        /// <summary>
        /// 构建成员访问表达式
        /// </summary>
        /// <param name="methodSymbol"></param>
        /// <returns></returns>
        private Expression GetOperatorMemberAccessExpression(IMethodSymbol methodSymbol) {
            // 处理成员名，并返回methodSymbol的名称
            var methodName = _generator.GetMemberName(methodSymbol);
            // 如果该方法的所属类型是当前类型符号，直接返回方法名(直接由curTypeSymbol调用)
            if (SymbolEqualityComparer.Default.Equals(CurTypeSymbol, methodSymbol.ContainingType)) {
                return methodName;
            }

            // 判断当前类型符号是否包含该方法
            if (IsContainsInternalSymbol(CurTypeSymbol, methodSymbol)) {
                // 该方法名在当前类型符号中不存在，返回方法名
                if (CurTypeSymbol.GetMembers(methodSymbol.Name).IsEmpty) {
                    return methodName;
                }
            }

            // 构造方法的上一层调用该方法的表达式
            var typeName = _generator.GetTypeName(methodSymbol.ContainingType);
            return typeName.MemberAccess(methodName);
        }
        
        /// <summary>
        /// 判断symbol是否是type包含在内部的符号
        /// </summary>
        /// <param name="type"></param>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public static bool IsContainsInternalSymbol(INamedTypeSymbol type, ISymbol symbol) {
            if (SymbolEqualityComparer.Default.Equals(type, symbol.ContainingType)) {
                return true;
            }

            var containingType = type.ContainingType;
            // 如果containingType不是泛型，继续判断
            if (containingType?.IsGenericType == false) {
                return IsContainsInternalSymbol(containingType, symbol);
            }

            return false;
        }

        /// <summary>
        /// 判断是否是用户定义或隐式转换
        /// </summary>
        /// <param name="semanticModel"></param>
        /// <param name="node"></param>
        /// <param name="methodSymbol"></param>
        /// <returns></returns>
        public static bool IsUserConversion(SemanticModel semanticModel, ExpressionSyntax node, out IMethodSymbol methodSymbol) {
            var conversion = semanticModel.GetConversion(node);
            if (conversion.IsUserDefined && conversion.IsImplicit) {
                // 返回用于转换的方法
                methodSymbol = conversion.MethodSymbol;
                return true;
            }

            methodSymbol = null;
            return false;
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
                    if (prefix != LuaDefine.IdentifierName.System.name &&
                        prefix != LuaDefine.IdentifierName.Class.name) {
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
            if (!IsNoImportTypeName && !SymbolEqualityComparer.Default.Equals(CurTypeSymbol, symbol) &&
                !IsCurMethodTypeArgument(symbol)) {
                var invocationExpression = (InvocationExpression)luaExpression;
                string newName = GetGenericTypeImportName(invocationExpression, out var argumentTypeNames);
                // 判断是否是本地变量
                if (!IsLocalVarExistsInCurMethod(newName)) {
                    bool success;
                    // 判断是否存在参数类型
                    if (!IsTypeParameterExists(symbol)) {
                        // 没有参数，直接添加泛型导入
                        success = AddGenericImport(invocationExpression, newName, argumentTypeNames,
                            IsAbsoluteFromCode(symbol));
                    }
                    else {
                        success = CurTypeDeclaration.TypeDeclaration.AddGenericImport(invocationExpression, newName,
                            argumentTypeNames, IsAbsoluteFromCode(symbol), out var declare);
                        // declare为null说明已经添加过
                        if (declare != null) {
                            // 添加泛型的依赖
                            bool hasAdd = _generator.AddGenericImportDepend(CurTypeDeclaration.TypeSymbol,
                                symbol.OriginalDefinition as INamedTypeSymbol);
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
        bool AddGenericImport(InvocationExpression invocationExpression, string name, List<string> argumentTypeNames,
            bool isFromCode) {
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

        private static string GetGenericTypeImportName(InvocationExpression invocationExpression,
            out List<string> argumentTypeNames) {
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
        private static void FillGenericTypeImportName(StringBuilder sb, List<string> argumentTypeNames,
            InvocationExpression invocationExpression) {
            var identifierName = (IdentifierNameExpression)invocationExpression.expression;
            sb.Append(CheckLastName(LastName(identifierName.name)));
            foreach (var argument in invocationExpression.arguments) {
                if (argument is IdentifierNameExpression typeName) {
                    string argumentTypeName = typeName.name;
                    sb.Append(CheckLastName(LastName(argumentTypeName)));
                    argumentTypeNames.Add(argumentTypeName);
                }
                else {
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
                return method.TypeArguments.Any(i => IsTypeParameterExists(i));
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
                var statement =
                    curBlock.statements.FindLast(i => i is not BlankLineStatement && i is not CommentStatement);
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
                        return new CharExpression(((int)constantValue).ToString(),
                            SyntaxFactory.Literal((char)constantValue).Text);
                    }
                    case TypeCode.String: {
                        return BuildStringLiteralExpression((string)constantValue);
                    }
                    case TypeCode.Boolean: {
                        bool v = (bool)constantValue;
                        return v
                            ? new ValExpression(LuaDefine.IdentifierName.True.name, LuaDefine.IdentifierName.True)
                            : new ValExpression(LuaDefine.IdentifierName.False.name, LuaDefine.IdentifierName.False);
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
                            const long kMinInteger = long.MinValue + 1; // in lua5.4 long.MinValue will be float
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