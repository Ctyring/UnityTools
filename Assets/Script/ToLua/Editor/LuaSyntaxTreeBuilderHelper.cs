using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using CSharpLua;
using CSharpLua.LuaAst;
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

        private sealed class RefOrOutArgument {
            public Expression Expression { get; }
            public bool IsDeclaration { get; }
            public bool IsSpecial { get; }

            public RefOrOutArgument(Expression expression) {
                Expression = expression;
            }

            public RefOrOutArgument(Expression expression, ArgumentSyntax argument) {
                Expression = expression;
                IsSpecial = IsInSpecialBinaryExpression(argument);
                IsDeclaration = (argument.Expression.IsKind(SyntaxKind.DeclarationExpression) && !IsSpecial) ||
                                expression == LuaDefine.IdentifierName.Placeholder;
            }

            private static bool IsInSpecialBinaryExpression(ArgumentSyntax argument) {
                if (argument.Expression.IsKind(SyntaxKind.DeclarationExpression)) {
                    var invocationExpression = (InvocationExpressionSyntax)argument.Parent.Parent;
                    var parent = invocationExpression.Parent;
                    if (parent.IsKind(SyntaxKind.LogicalAndExpression) ||
                        parent.IsKind(SyntaxKind.LogicalOrExpression)) {
                        var binaryExpression = (BinaryExpressionSyntax)parent;
                        if (binaryExpression.Right == invocationExpression) {
                            return true;
                        }
                    }
                }

                return false;
            }
        }

        /// <summary>
        /// 获取对象创建表达式
        /// </summary>
        /// <param name="creationExpression"></param>
        /// <param name="node"></param>
        /// <returns></returns>
        private Expression GetObjectCreationInitializer(Expression creationExpression,
            BaseObjectCreationExpressionSyntax node) {
            if (node.Initializer == null) {
                return creationExpression;
            }

            return GetObjectCreationInitializer(creationExpression, node.Initializer, node);
        }

        /// <summary>
        /// 对象创建初始化
        /// </summary>
        /// <param name="creationExpression"></param>
        /// <param name="initializer"></param>
        /// <param name="node"></param>
        /// <returns></returns>
        private Expression GetObjectCreationInitializer(Expression creationExpression,
            InitializerExpressionSyntax initializer, ExpressionSyntax node) {
            int prevTempCount = CurFunction.TempCount;
            int prevReleaseCount = CurBlock.ReleaseCount;
            // 构建一个临时变量名
            var temp = GetTempIdentifier();
            // 把creationExpression声明一下
            CurBlock.AddStatement(new LocalVariableDeclaratorSyntax(temp, creationExpression));
            // 在CurBlock中添加对象初始化需要的语句
            FillObjectInitializerExpression(temp, initializer);
            var grandparent = initializer!.Parent!.Parent;
            if (grandparent != null) {
                switch (grandparent.Kind()) {
                    case SyntaxKind.Argument:
                    case SyntaxKind.ArrayInitializerExpression:
                        break;
                    default:
                        // 释放临时变量
                        ReleaseTempIdentifiers(prevTempCount, prevReleaseCount);
                        break;
                }
            }

            return !node.Parent.IsKind(SyntaxKind.ExpressionStatement) ? temp : Expression.EmptyExpression;
        }

        private void ReleaseTempIdentifiers(int prevTempCount, int prevReleaseCount) {
            int count = CurFunction.TempCount - prevTempCount;
            PopTempCount(count);
            CurBlock.ReleaseCount = prevReleaseCount;
        }

        private void PopTempCount(int count) {
            Contract.Assert(CurBlock.TempCount >= count && CurFunction.TempCount >= count);
            CurBlock.TempCount -= count;
            CurFunction.TempCount -= count;
        }

        /// <summary>
        /// 填充对象初始化器表达式
        /// </summary>
        /// <param name="temp"></param>
        /// <param name="node"></param>
        private void FillObjectInitializerExpression(IdentifierNameExpression temp, InitializerExpressionSyntax node) {
            // 处理初始化对象需要的每个表达式
            foreach (var expression in node.Expressions) {
                // 赋值表达式
                if (expression.IsKind(SyntaxKind.SimpleAssignmentExpression)) {
                    var assignment = (AssignmentExpressionSyntax)expression;
                    var left = assignment.Left.Accept(this);
                    // 如果右侧是集合初始化器或者对象初始化器
                    if (assignment.Right.IsKind(SyntaxKind.CollectionInitializerExpression) ||
                        assignment.Right.IsKind(SyntaxKind.ObjectInitializerExpression)) {
                        var rightNode = (InitializerExpressionSyntax)assignment.Right;
                        if (rightNode.Expressions.Count > 0) {
                            var leftExpression = (Expression)left;
                            // 先把右侧的表达式构建出来
                            GetObjectCreationInitializer(
                                new MemberAccessExpression(temp, leftExpression,
                                    leftExpression is not IdentifierNameExpression), rightNode, expression);
                        }
                    }
                    else {
                        var right = (Expression)assignment.Right.Accept(this);
                        // 如果左侧是隐式元素访问
                        if (assignment.Left.IsKind(SyntaxKind.ImplicitElementAccess)) {
                            // 获取隐式元素访问的表达式
                            var argumentList = (ArgumentListSyntaxNode)left;
                            IdentifierNameExpression methodName = LuaSyntaxNode.Tokens.Set;
                            var invocation = temp.MemberAccess(methodName, true).Invocation();
                            invocation.arguments.Arguments.AddRange(argumentList!.Arguments);
                            invocation.AddArgument(right);
                            // 加入当前block
                            CurBlock.AddStatement(invocation);
                        }
                        else {
                            // 构建成员访问表达式
                            var memberAccess =
                                BuildFieldOrPropertyMemberAccessExpression(temp, (Expression)left, false);
                            // 构建赋值表达式
                            var assignmentExpression = BuildLuaSimpleAssignmentExpression(memberAccess, right);
                            CurBlock.AddStatement(assignmentExpression);
                        }
                    }
                }
                else {
                    // 如果是集合初始化器
                    var symbol = _semanticModel.GetCollectionInitializerSymbolInfo(expression).Symbol;
                    var name = _generator.GetMemberName(symbol);
                    var invocation = temp.MemberAccess(name, true).Invocation();
                    // 处理内部
                    var block = new BlockStatement();
                    PushBlock(block);
                    // 是否是复杂元素初始化器
                    if (expression.IsKind(SyntaxKind.ComplexElementInitializerExpression)) {
                        // 逐个加入参数列表
                        var initializer = (InitializerExpressionSyntax)expression;
                        foreach (var expressionNode in initializer.Expressions) {
                            var argument = (Expression)expressionNode.Accept(this);
                            invocation.AddArgument(argument);
                        }
                    }
                    else {
                        // 直接加入参数列表
                        var value = (Expression)expression.Accept(this);
                        invocation.AddArgument(value);
                    }

                    PopBlock();
                    CurBlock.statements.AddRange(block.statements);
                    CurBlock.AddStatement(invocation);
                }
            }
        }

        public void PushBlock(BlockStatement block) {
            _blocks.Push(block);
        }

        public void PopBlock() {
            var block = _blocks.Pop();
            if (block.TempCount > 0) {
                Contract.Assert(CurFunction.TempCount >= block.TempCount);
                CurFunction.TempCount -= block.TempCount;
            }
        }

        /// <summary>
        /// 构建二元赋值调用表达式
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <returns></returns>
        private Expression BuildLuaSimpleAssignmentExpression(Expression left, Expression right) {
            if (left is PropertyAdapterExpression propertyAdapter) {
                propertyAdapter.IsGetOrAdd = false;
                propertyAdapter.ArgumentList.AddArgument(right);
                return propertyAdapter;
            }

            return left.Assignment(right);
        }

        private Expression BuildBinaryInvokeAssignmentExpression(ExpressionSyntax leftNode, ExpressionSyntax rightNode,
            Expression methodName) {
            var left = (Expression)leftNode.Accept(this);
            var right = (Expression)rightNode.Accept(this);
            return BuildBinaryInvokeAssignmentExpression(left, right, methodName);
        }

        /// <summary>
        /// 构建二元赋值调用表达式
        /// </summary>
        /// <param name="left"></param>
        /// <param name="right"></param>
        /// <param name="methodName"></param>
        /// <returns></returns>
        private Expression BuildBinaryInvokeAssignmentExpression(Expression left, Expression right,
            Expression methodName) {
            // 判断是否是属性访问
            if (left is PropertyAdapterExpression propertyAdapter) {
                var invocation = new InvocationExpression(methodName, propertyAdapter.GetCloneOfGet(), right);
                propertyAdapter.ArgumentList.AddArgument(invocation);
                return propertyAdapter;
            }
            else {
                var invocation = new InvocationExpression(methodName, left, right);
                return left.Assignment(invocation);
            }
        }

        /// <summary>
        /// 构建字段或属性成员访问
        /// </summary>
        /// <param name="expression"></param>
        /// <param name="name"></param>
        /// <param name="isStatic"></param>
        /// <returns></returns>
        private Expression BuildFieldOrPropertyMemberAccessExpression(Expression expression, Expression name,
            bool isStatic) {
            // 属性处理
            if (name is PropertyAdapterExpression propertyMethod) {
                var arguments = propertyMethod.ArgumentList.Arguments;
                if (arguments.Count == 1) {
                    // 处理this
                    if (arguments[0] == LuaDefine.IdentifierName.This) {
                        propertyMethod.ArgumentList.Arguments[0] = expression;
                    }
                }
                else {
                    // 如果只有一个参数，判断是否添加this(是否处理成:符号访问)
                    propertyMethod.Update(expression, !isStatic);
                }

                return propertyMethod;
            }

            // 字段处理
            return expression.MemberAccess(name);
        }

        /// <summary>
        /// 获取临时name表达式
        /// </summary>
        /// <returns></returns>
        private IdentifierNameExpression GetTempIdentifier() {
            int index = CurFunction.TempCount++;
            string name = default;
            // 判断是否有现成可以服用的
            if (index < LuaSyntaxNode.TempIdentifiers.Length) {
                name = LuaSyntaxNode.TempIdentifiers[index];
            }
            else {
                name = $"__temp{index - LuaSyntaxNode.TempIdentifiers.Length}__";
            }

            ++CurBlock.TempCount;
            return name;
        }

        /// <summary>
        /// 构建代码模板
        /// </summary>
        /// <returns></returns>
        private ListExpression BuildCodeTemplate(string codeTemplate,
            IdentifierNameExpression memberBindingIdentifierName,
            IList<ITypeSymbol> typeArguments, IEnumerable<Func<Expression>> arguments,
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
        private List<Func<Expression>> FillCodeTemplateInvocationArguments(IMethodSymbol symbol,
            ArgumentListSyntax argumentList, List<Func<Expression>> argumentExpressions) {
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
                    }
                    else {
                        // 扩容
                        int count = index - argumentExpressions.Count;
                        for (int i = 0; i < count; ++i) {
                            argumentExpressions.Add(default);
                        }

                        argumentExpressions.Add(() => VisitExpression(argument.Expression));
                    }
                }
                else {
                    // 正常的参数，就直接遍历处理
                    argumentExpressions.Add(() => VisitExpression(argument.Expression));
                }
            }

            // 填充默认参数
            for (int i = 0; i < argumentExpressions.Count; ++i) {
                argumentExpressions[i] ??=
                    () => GetDefaultParameterValue(symbol.Parameters[i], argumentList.Parent, true);
            }

            // 在末尾填充默认参数
            if (symbol.Parameters.Length > argumentList.Arguments.Count) {
                argumentExpressions.AddRange(symbol.Parameters.Skip(argumentList.Arguments.Count)
                    .Where(i => !i.IsParams).Select(i => {
                        Expression Func() => GetDefaultParameterValueExpression(i, argumentList, true);
                        return (Func<Expression>)Func;
                    }));
            }

            return argumentExpressions;
        }

        Expression GetDefaultParameterValueExpression(IParameterSymbol i, ArgumentListSyntax argumentList,
            bool isCheckCallerAttribute) {
            return GetDefaultParameterValue(i, argumentList.Parent, true);
        }

        private Expression GetDefaultParameterValue(IParameterSymbol parameter, SyntaxNode node,
            bool isCheckCallerAttribute) {
            Contract.Assert(parameter.HasExplicitDefaultValue);
            Expression defaultValue = isCheckCallerAttribute ? CheckCallerAttribute(parameter, node) : null;
            if (defaultValue == null) {
                if (parameter.ExplicitDefaultValue == null && parameter.Type.IsValueType) {
                    defaultValue = GetDefaultValueExpression(parameter.Type);
                }
                else {
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

        /// <summary>
        /// 获取lua类型的默认值
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <returns></returns>
        private Expression GetPredefinedValueTypeDefaultValue(ITypeSymbol typeSymbol) {
            switch (typeSymbol.SpecialType) {
                case SpecialType.None: {
                    if (typeSymbol.TypeKind == TypeKind.Enum) {
                        if (!_generator.IsConstantEnum(typeSymbol)) {
                            return BuildEnumNoConstantDefaultValue(typeSymbol);
                        }

                        return ValExpression.Zero;
                    }

                    if (IsTimeSpanType(typeSymbol)) {
                        return BuildDefaultValue(LuaDefine.IdentifierName.TimeSpan);
                    }

                    return null;
                }
                case SpecialType.System_Boolean: {
                    return new IdentifierExpression(LuaDefine.IdentifierName.False);
                }
                case SpecialType.System_Char: {
                    return new CharExpression(default);
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

        public static bool IsTimeSpanType(ITypeSymbol typeSymbol) {
            return typeSymbol.IsValueType && typeSymbol.ContainingNamespace.Name == "System" &&
                   typeSymbol.Name == "TimeSpan";
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
                invokeExpression.arguments.Arguments.AddRange(arguments);
                creationExpression = refOrOutArguments.Count > 0
                    ? BuildInvokeRefOrOut(node, invokeExpression, refOrOutArguments)
                    : invokeExpression;
            }
            else {
                creationExpression = invokeExpression;
            }

            return creationExpression;
        }

        private Expression BuildInvokeRefOrOut(CSharpSyntaxNode node, Expression invocation,
            IEnumerable<RefOrOutArgument> refOrOutArguments) {
            var locals = new LocalVariables();
            var multipleAssignment = new MultipleAssignmentExpression();
            var propertyStatements = new StatementList();

            void FillRefOrOutArguments() {
                foreach (var refOrOutArgument in refOrOutArguments) {
                    // fn(out arr[0])
                    if (refOrOutArgument.Expression is PropertyAdapterExpression propertyAdapter) {
                        var propertyTemp = GetTempIdentifier();
                        locals.Variables.Add(propertyTemp);
                        multipleAssignment.Lefts.Add(propertyTemp);

                        var setPropertyAdapter = propertyAdapter.GetClone();
                        setPropertyAdapter.IsGetOrAdd = false;
                        setPropertyAdapter.ArgumentList.AddArgument(propertyTemp);
                        propertyStatements.Statements.Add(setPropertyAdapter);
                    }
                    else {
                        if (refOrOutArgument.IsDeclaration) {
                            locals.Variables.Add((IdentifierNameExpression)refOrOutArgument.Expression);
                        }
                        else if (refOrOutArgument.IsSpecial) {
                            CurFunction.body.AddHeadVariable((IdentifierNameExpression)refOrOutArgument.Expression);
                        }

                        multipleAssignment.Lefts.Add(refOrOutArgument.Expression);
                    }
                }
            }

            switch (node.Parent.Kind()) {
                case SyntaxKind.ConditionalAccessExpression:
                case SyntaxKind.ExpressionStatement:
                case SyntaxKind.ConstructorDeclaration: {
                    var symbol = (IMethodSymbol)_semanticModel.GetSymbolInfo(node).Symbol;
                    if (!symbol.ReturnsVoid || node.IsKind(SyntaxKind.ObjectCreationExpression)) {
                        var temp = node.Parent.IsKind(SyntaxKind.ExpressionStatement)
                            ? LuaDefine.IdentifierName.Placeholder
                            : GetTempIdentifier();
                        locals.Variables.Add(temp);
                        multipleAssignment.Lefts.Add(temp);
                    }

                    FillRefOrOutArguments();
                    multipleAssignment.Rights.Add(invocation);

                    if (locals.Variables.Count > 0) {
                        CurBlock.statements.Add(locals);
                        if (locals.Variables.SequenceEqual(multipleAssignment.Lefts)) {
                            locals.Initializer = new EqualsValueClauseList(multipleAssignment.Rights);
                            if (propertyStatements.Statements.Count > 0) {
                                CurBlock.statements.Add(propertyStatements);
                            }

                            return Expression.EmptyExpression;
                        }
                    }

                    if (propertyStatements.Statements.Count > 0) {
                        CurBlock.statements.Add(multipleAssignment);
                        CurBlock.statements.Add(propertyStatements);
                        return Expression.EmptyExpression;
                    }

                    return multipleAssignment;
                }
                default: {
                    bool isReturnsVoid = false;
                    if (node.Parent.IsKind(SyntaxKind.ArrowExpressionClause)) {
                        isReturnsVoid = CurMethodInfoOrNull?.Symbol.ReturnsVoid == true;
                    }

                    var temp = !isReturnsVoid ? GetTempIdentifier() : LuaDefine.IdentifierName.Placeholder;
                    locals.Variables.Add(temp);
                    multipleAssignment.Lefts.Add(temp);
                    FillRefOrOutArguments();
                    multipleAssignment.Rights.Add(invocation);
                    CurBlock.statements.Add(locals);
                    if (locals.Variables.SequenceEqual(multipleAssignment.Lefts)) {
                        locals.Initializer = new EqualsValueClauseList(multipleAssignment.Rights);
                    }
                    else {
                        CurBlock.statements.Add(multipleAssignment);
                    }

                    if (propertyStatements.Statements.Count > 0) {
                        CurBlock.statements.Add(propertyStatements);
                    }

                    return !isReturnsVoid ? temp : Expression.EmptyExpression;
                }
            }
        }

        private void TryRemoveNilArgumentsAtTail(ISymbol symbol, List<Expression> arguments) {
            if (arguments.Count > 0) {
                if (_generator.IsFromLuaModule(symbol)) {
                    RemoveNilAtTail(arguments);
                }
            }
        }

        public static void RemoveNilAtTail(List<Expression> expressions) {
            int pos = expressions.FindLastIndex(i => !IsNil(i));
            int nilStartIndex = pos + 1;
            int nilArgumentCount = expressions.Count - nilStartIndex;
            if (nilArgumentCount > 0) {
                expressions.RemoveRange(nilStartIndex, nilArgumentCount);
            }
        }

        public static bool IsNil(Expression expression) {
            return expression == null || expression == LuaDefine.IdentifierName.Nil ||
                   expression == IdentifierExpression.Nil;
        }

        private List<Expression> BuildArgumentList(ISymbol symbol, ImmutableArray<IParameterSymbol> parameters,
            BaseArgumentListSyntax node, List<RefOrOutArgument> refOrOutArguments = null) {
            Contract.Assert(node != null);
            List<Expression> arguments = new List<Expression>();
            foreach (var argument in node.Arguments) {
                FillInvocationArgument(arguments, argument, parameters, refOrOutArguments);
            }

            CheckInvocationDefaultArguments(symbol, parameters, arguments, node);
            return arguments;
        }

        private void CheckInvocationDefaultArguments(ISymbol symbol, ImmutableArray<IParameterSymbol> parameters,
            List<Expression> arguments, BaseArgumentListSyntax node) {
            var argumentNodeInfos = node.Arguments.Select(i => (i.NameColon, i.Expression)).ToList();
            CheckInvocationDefaultArguments(symbol, parameters, arguments, argumentNodeInfos, node.Parent, true);
        }

        private void CheckInvocationDefaultArguments(
            ISymbol symbol,
            ImmutableArray<IParameterSymbol> parameters,
            List<Expression> arguments,
            List<(NameColonSyntax Name, ExpressionSyntax Expression)> argumentNodeInfos,
            SyntaxNode node,
            bool isCheckCallerAttribute) {
            if (parameters.Length > arguments.Count) {
                var optionalParameters = parameters.Skip(arguments.Count);
                foreach (IParameterSymbol parameter in optionalParameters) {
                    if (parameter.IsParams) {
                        var arrayType = (IArrayTypeSymbol)parameter.Type;
                        var elementType = _generator.GetTypeName(arrayType.ElementType);
                        arguments.Add(LuaDefine.IdentifierName.EmptyArray.Invocation(elementType));
                    }
                    else {
                        Expression defaultValue =
                            GetDefaultParameterValue(parameter, node, isCheckCallerAttribute);
                        arguments.Add(defaultValue);
                    }
                }
            }
            else if (!parameters.IsEmpty) {
                IParameterSymbol last = parameters.Last();
                if (last.IsParams && _generator.IsFromLuaModule(symbol) && !HasParamsAttribute(symbol)) {
                    if (parameters.Length == arguments.Count) {
                        var paramsArgument = argumentNodeInfos.Last();
                        if (paramsArgument.Name != null) {
                            string name = paramsArgument.Name.Name.Identifier.ValueText;
                            if (name != last.Name) {
                                paramsArgument = argumentNodeInfos.First(i =>
                                    i.Name != null && i.Name.Name.Identifier.ValueText == last.Name);
                            }
                        }

                        var paramsType = _semanticModel.GetTypeInfo(paramsArgument.Expression).Type;
                        bool isLastParamsArrayType = paramsType is { TypeKind: TypeKind.Array };
                        if (!isLastParamsArrayType) {
                            var arrayTypeSymbol = (IArrayTypeSymbol)last.Type;
                            var array = BuildArray(arrayTypeSymbol, arguments.Last());
                            arguments[^1] = array;
                        }
                    }
                    else {
                        int otherParameterCount = parameters.Length - 1;
                        var arrayTypeSymbol = (IArrayTypeSymbol)last.Type;
                        var paramsArguments = arguments.Skip(otherParameterCount).ToArray();
                        var array = BuildArray(arrayTypeSymbol, paramsArguments);
                        arguments.RemoveRange(otherParameterCount, arguments.Count - otherParameterCount);
                        arguments.Add(array);
                    }
                }
            }

            for (int i = 0; i < arguments.Count; ++i) {
                if (arguments[i] == null) {
                    Expression defaultValue = GetDefaultParameterValue(parameters[i], node, isCheckCallerAttribute);
                    arguments[i] = defaultValue;
                }
            }
        }
        
        private Expression BuildArray(IArrayTypeSymbol symbol, params Expression[] elements) {
            var baseType = _generator.GetTypeName(symbol.ElementType);
            var arrayType = new InvocationExpression(LuaDefine.IdentifierName.Array, baseType);
            return BuildArray(symbol, arrayType, elements);
        }
        
        private Expression BuildArray(IArrayTypeSymbol symbol, Expression arrayType, IList<Expression> elements) {
            var invocation = new InvocationExpression(arrayType);
            var table = new LuaTableExpression(elements);
            bool isElementNotNull = (symbol.ElementType.IsValueType && !IsNullableType(symbol.ElementType)) 
                                    || elements.All(i => i is ValExpression && i != IdentifierExpression.Nil);
            if (isElementNotNull) {
                invocation.AddArgument(table);
                invocation.arguments.IsCallSingleTable = true;
            } else {
                invocation.AddArgument(elements.Count);
                invocation.AddArgument(table);
            }
            table.IsSingleLine = elements.All(i => i is ValExpression || i is IdentifierNameExpression);
            return invocation;
        }
        
        public static bool HasParamsAttribute(ISymbol symbol) {
            var node = GetDeclaringSyntaxNode(symbol);
            if (node != null) {
                return LuaGenerator.HasCSharpLuaAttribute(node, DocumentStatement.AttributeFlags.Params);
            }
            return false;
        }

        private void FillInvocationArgument(List<Expression> arguments, ArgumentSyntax node,
            ImmutableArray<IParameterSymbol> parameters, List<RefOrOutArgument> refOrOutArguments) {
            var expression = (Expression)node.Expression.Accept(this);
            Contract.Assert(expression != null);
            if (node.RefKindKeyword.IsKind(SyntaxKind.RefKeyword)) {
                refOrOutArguments.Add(new RefOrOutArgument(expression));
            }
            else if (node.RefKindKeyword.IsKind(SyntaxKind.OutKeyword)) {
                refOrOutArguments.Add(new RefOrOutArgument(expression, node));
                expression = LuaDefine.IdentifierName.Nil;
            }
            else {
                CheckConversion(node.Expression, ref expression);
            }

            if (node.NameColon != null) {
                string name = node.NameColon.Name.Identifier.ValueText;
                int index = IndexOf(parameters, i => i.Name == name);
                if (index == -1) {
                    throw new InvalidOperationException();
                }

                AddAt(arguments, index, expression);
            }
            else {
                arguments.Add(expression);
            }
        }
        
        public static int IndexOf<T>(IEnumerable<T> source, Predicate<T> match) {
            int index = 0;
            foreach (var item in source) {
                if (match(item)) {
                    return index;
                }
                ++index;
            }
            return -1;
        }
        
        public static void AddAt<T>(IList<T> list, int index, T v) {
            if (index < list.Count) {
                list[index] = v;
            } else {
                int count = index - list.Count;
                for (int i = 0; i < count; ++i) {
                    list.Add(default);
                }
                list.Add(v);
            }
        }

        private InvocationExpression BuildObjectCreationInvocation(IMethodSymbol symbol, Expression expression) {
            int constructorIndex = GetConstructorIndex(symbol);
            if (constructorIndex > 1) {
                return new InvocationExpression(LuaDefine.IdentifierName.SystemNew, expression,
                    constructorIndex.ToString());
            }

            return new InvocationExpression(expression);
        }

        public int GetConstructorIndex(IMethodSymbol symbol) {
            Contract.Assert(symbol.MethodKind == MethodKind.Constructor);
            if (_generator.IsFromLuaModule(symbol.ContainingType)) {
                var typeSymbol = (INamedTypeSymbol)symbol.ReceiverType;
                if (typeSymbol.InstanceConstructors.Length > 1) {
                    var ctors = typeSymbol.InstanceConstructors.ToList();
                    int firstCtorIndex;
                    if (typeSymbol.IsValueType) {
                        Contract.Assert(ctors.Last().IsImplicitlyDeclared);
                        firstCtorIndex = IndexOf(ctors, i => IsNotNullParameterExists(i));
                        if (firstCtorIndex == -1) {
                            firstCtorIndex = ctors.Count - 1;
                        }
                        else if (symbol.IsImplicitlyDeclared) {
                            return 1;
                        }
                    }
                    else {
                        if (typeSymbol.IsRecord) {
                            int posIndex = ctors.FindIndex(i =>
                                i.Parameters.Length == 1 && SymbolEqualityComparer.Default.Equals(i.Parameters[0].Type, typeSymbol));
                            Contract.Assert(posIndex != -1);
                            ctors.RemoveAt(posIndex);
                            if (ctors.Count <= 1) {
                                return 0;
                            }
                        }

                        firstCtorIndex = IndexOf(ctors, i => i.Parameters.IsEmpty);
                    }

                    if (firstCtorIndex != -1 && firstCtorIndex != 0) {
                        var firstCtor = ctors[firstCtorIndex];
                        ctors.Remove(firstCtor);
                        ctors.Insert(0, firstCtor);
                    }

                    int index = ctors.IndexOf(symbol);
                    Contract.Assert(index != -1);
                    int ctorCounter = index + 1;
                    return ctorCounter;
                }
            }

            return 0;
        }
        
        public static bool IsNotNullParameterExists(IMethodSymbol symbol) {
            return FindNotNullParameterIndex(symbol.OriginalDefinition) != -1;
        }
        
        private static int FindNotNullParameterIndex(IMethodSymbol symbol) {
            return IndexOf(symbol.Parameters, i => i.Type.IsValueType && i.RefKind != RefKind.Out);
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
                    return new StringExpression(memberName);
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

        private static SyntaxNode FindParent(SyntaxNode node, Func<SyntaxNode, bool> match) {
            var parent = node.Parent;
            while (true) {
                if (match(parent)) {
                    return parent;
                }

                parent = parent.Parent;
            }
        }

        private CallerAttributeKind GetCallerAttributeKind(IParameterSymbol parameter) {
            foreach (var attribute in parameter.GetAttributes()) {
                var callerKind = GetCallerAttributeKind(attribute.AttributeClass);
                if (callerKind != CallerAttributeKind.None) {
                    return callerKind;
                }
            }

            return CallerAttributeKind.None;
        }

        private CallerAttributeKind GetCallerAttributeKind(INamedTypeSymbol symbol) {
            if (IsRuntimeCompilerServices(symbol.ContainingNamespace)) {
                return symbol.Name switch {
                    "CallerLineNumberAttribute" => CallerAttributeKind.Line,
                    "CallerMemberNameAttribute" => CallerAttributeKind.Member,
                    "CallerFilePathAttribute" => CallerAttributeKind.FilePath,
                    "CallerArgumentExpressionAttribute" => CallerAttributeKind.ArgumentExpression,
                    _ => CallerAttributeKind.None
                };
            }

            return CallerAttributeKind.None;
        }

        public static bool IsRuntimeCompilerServices(INamespaceSymbol symbol) {
            return symbol.Name == "CompilerServices" && symbol.ContainingNamespace.Name == "Runtime" &&
                   symbol.ContainingNamespace.ContainingNamespace.Name == "System";
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
                var args = new[] { expression };
                // 构建
                return BuildCodeTemplate(codeTemplate, null, null,
                    args.Select<Expression, Func<Expression>>(i => () => i), null);
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
        public static bool IsUserConversion(SemanticModel semanticModel, ExpressionSyntax node,
            out IMethodSymbol methodSymbol) {
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
            foreach (var argument in invocationExpression.arguments.Arguments) {
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
                        return new CharExpression((char)constantValue);
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