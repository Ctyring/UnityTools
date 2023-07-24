using System;
using System.CodeDom;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using CSharpLua;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;
using UnityEngine;

namespace Script.ToLua.Editor
{
    public class LuaGenerator
    {
        CSharpCompilation _compilation; // 编译器
        private List<INamedTypeSymbol> _types; // 类型名

        Dictionary<INamedTypeSymbol, HashSet<INamedTypeSymbol>> _typeDependence =
            new Dictionary<INamedTypeSymbol, HashSet<INamedTypeSymbol>>(); // 类型依赖

        Dictionary<INamedTypeSymbol, HashSet<INamedTypeSymbol>> _implicitExtends = new(SymbolEqualityComparer.Default);

        private Dictionary<ISymbol, HashSet<ISymbol>> _implicitInterfaceImplementations =
            new(SymbolEqualityComparer.Default);

        private Dictionary<INamedTypeSymbol, Dictionary<ISymbol, ISymbol>> _implicitInterfaceTypes =
            new(SymbolEqualityComparer.Default);

        private HashSet<INamedTypeSymbol> _typesOfExtendSelf = new(SymbolEqualityComparer.Default);
        private Dictionary<INamedTypeSymbol, string> _typeRefactorNames = new(SymbolEqualityComparer.Default);
        Dictionary<INamespaceSymbol, string> _namespaceRefactorNames = new(SymbolEqualityComparer.Default);
        public XmlMetaProvider metaProvider;
        private string metas = "";
        public ImmutableList<Expression> assemblyAttributes = ImmutableList<Expression>.Empty;

        private ConcurrentDictionary<INamedTypeSymbol, LuaTool.ConcurrentHashSet<INamedTypeSymbol>>
            _genericImportDepends = new(SymbolEqualityComparer.Default);

        LuaTool.ConcurrentHashSet<string> _exportEnums = new();
        LuaTool.ConcurrentHashSet<ITypeSymbol> _ignoreExportTypes = new(SymbolEqualityComparer.Default);
        Dictionary<ISymbol, IdentifierNameExpression> _memberNames = new(SymbolEqualityComparer.Default);

        private ConcurrentDictionary<IEventSymbol, bool>
            _isFieldEvents = new ConcurrentDictionary<IEventSymbol, bool>();
        ConcurrentDictionary<IPropertySymbol, bool> _isFieldProperties = new(SymbolEqualityComparer.Default);
        Dictionary<INamedTypeSymbol, HashSet<string>> _typeUsedNames = new(SymbolEqualityComparer.Default);
        HashSet<ISymbol> _refactorNames = new(SymbolEqualityComparer.Default);
        Dictionary<ISymbol, string> _memberIllegalNames = new(SymbolEqualityComparer.Default);

        public SettingInfo Setting { get; set; }

        public void Init()
        {
            BuildCompilation();
            metaProvider = new XmlMetaProvider(LoadMeta(LuaTool.Split(metas)));
        }

        public void CreateLua()
        {
            foreach (SyntaxTree tree in _compilation.SyntaxTrees)
            {
                CreateLuaCompilationUnitSyntax(tree);
            }
        }

        Thunk CreateLuaCompilationUnitSyntax(SyntaxTree tree)
        {
            var semanticModel = _compilation.GetSemanticModel(tree);
            var syntax = (CompilationUnitSyntax) tree.GetRoot();
            LuaSyntaxTreeBuilder builder = new LuaSyntaxTreeBuilder(this, semanticModel);
            return builder.BuildLuaThunk(syntax);
        }

        private string GetNamespaceNames(List<INamespaceSymbol> symbols)
        {
            List<string> names = new List<string>();
            foreach (INamespaceSymbol symbol in symbols)
            {
                string name = symbol.Name;
                if (_namespaceRefactorNames.TryGetValue(symbol, out string refactorName))
                    name = refactorName;
                names.Add(name);
            }

            return string.Join(".", names);
        }

        private string GetNamespaceMapName(INamespaceSymbol symbol, string original)
        {
            if (!symbol.DeclaringSyntaxReferences.IsEmpty)
            {
                return GetNamespaceNames(GetAllNamespaces(symbol));
            }

            return metaProvider.GetNamespaceMapName(symbol, original);
        }

        public static IEnumerable<INamespaceSymbol> InternalGetAllNamespaces(INamespaceSymbol symbol)
        {
            do
            {
                yield return symbol;
                symbol = symbol.ContainingNamespace;
            } while (!symbol.IsGlobalNamespace);
        }

        public static List<INamespaceSymbol> GetAllNamespaces(INamespaceSymbol symbol)
        {
            if (symbol.IsGlobalNamespace)
            {
                return new List<INamespaceSymbol>();
            }

            return InternalGetAllNamespaces(symbol).Reverse().ToList();
        }

        public string GetNamespaceDefineName(INamespaceSymbol symbol, BaseNamespaceDeclarationSyntax node)
        {
            string original = node.Name.ToString();
            if (original == symbol.Name)
            {
                return _namespaceRefactorNames.GetValueOrDefault(symbol, original);
            }

            var namespaces = new List<INamespaceSymbol>() {symbol};
            do
            {
                // 递归获取命名空间
                symbol = symbol.ContainingNamespace;
                namespaces.Add(symbol);
                List<INamespaceSymbol> symbols = namespaces;
                symbols.Reverse();
                string symbolsName = string.Join(".", symbols.Select(i => i.Name));
                if (symbolsName == original)
                {
                    return GetNamespaceNames(symbols);
                }
            } while (!symbol.IsGlobalNamespace);

            throw new InvalidOperationException();
        }

        public Expression GetTypeName(ISymbol symbol, LuaSyntaxTreeBuilder transform = null)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.TypeParameter:
                {
                    return symbol.Name;
                }
                // 数组类型
                case SymbolKind.ArrayType:
                {
                    var arrayType = (IArrayTypeSymbol) symbol;
                    transform?.AddGenericTypeCounter();
                    // 获取数组元素类型
                    var elementType = GetTypeName(arrayType.ElementType, transform);
                    transform?.SubGenericTypeCounter();
                    // 包装成调用表达式
                    var invocation = new InvocationExpression(LuaDefine.IdentifierName.Array, elementType);
                    if (arrayType.Rank > 1)
                    {
                        invocation.AddArgument(arrayType.Rank.ToString());
                    }

                    Expression luaExpression = invocation;
                    transform?.ImportGenericTypeName(ref luaExpression, arrayType);
                    return luaExpression;
                }
                // 指针
                case SymbolKind.PointerType:
                {
                    var pointType = (IPointerTypeSymbol) symbol;
                    // 获取指针指向的类型
                    var elementTypeExpression = GetTypeName(pointType.PointedAtType, transform);
                    Expression luaExpression =
                        new InvocationExpression(LuaDefine.IdentifierName.Array, elementTypeExpression);
                    transform?.ImportGenericTypeName(ref luaExpression, pointType);
                    return luaExpression;
                }
                // dynamic
                case SymbolKind.DynamicType:
                {
                    return LuaDefine.IdentifierName.Object;
                }
            }

            // 枚举
            var namedTypeSymbol = (INamedTypeSymbol) symbol;
            if (IsConstantEnum(namedTypeSymbol))
            {
                return GetTypeName(namedTypeSymbol.EnumUnderlyingType, transform);
            }

            // 委托
            if (namedTypeSymbol.TypeKind == TypeKind.Delegate)
            {
                // 判断是否来自元数据
                if (transform?.IsMetadataTypeName == true)
                {
                    var delegateMethod = namedTypeSymbol.DelegateInvokeMethod;
                    Contract.Assert(delegateMethod != null);
                    // 判断该委托有参数或者返回值
                    if (!delegateMethod.Parameters.IsEmpty || !delegateMethod.ReturnsVoid)
                    {
                        var arguments = delegateMethod.Parameters.Select(i => GetTypeName(i.Type, transform)).ToList();
                        var argument = delegateMethod.ReturnsVoid
                            ? LuaDefine.IdentifierName.SystemVoid
                            : GetTypeName(delegateMethod.ReturnType, transform);
                        arguments.Add(argument);
                        return new InvocationExpression(LuaDefine.IdentifierName.Delegate, arguments.ToArray());
                    }
                }

                return LuaDefine.IdentifierName.Delegate;
            }

            // 匿名类型
            if (namedTypeSymbol.IsAnonymousType)
            {
                return LuaDefine.IdentifierName.AnonymousType;
            }

            // 值元组类型
            if (namedTypeSymbol.IsTupleType)
            {
                return LuaDefine.IdentifierName.ValueTuple;
            }

            // 元组
            if (namedTypeSymbol.Name == "Tuple" && namedTypeSymbol.ContainingNamespace.Name == "System")
            {
                return LuaDefine.IdentifierName.Tuple;
            }

            // 如果不是容器内的泛型类型，判断是否是当前声明的class类型
            if (transform?.IsNoneGenericTypeCounter == true)
            {
                var curTypeDeclaration = transform.CurTypeDeclaration;
                if (curTypeDeclaration != null &&
                    curTypeDeclaration.CheckTypeName(namedTypeSymbol, out var classIdentifier))
                {
                    return classIdentifier;
                }
            }

            var typeName = GetTypeShortName(namedTypeSymbol, transform);
            var typeArguments = GetTypeArguments(namedTypeSymbol, transform);
            // 如果没有参数，直接返回
            if (typeArguments.Count == 0) {
                return typeName;
            }

            {
        
                // 如果忽略泛型，直接返回name的最后一段
                if (metaProvider.IsTypeIgnoreGeneric(namedTypeSymbol)) {
                    string name = typeName.value;
                    int genericTokenPos = name.LastIndexOf('_');
                    if (genericTokenPos != -1) {
                        return name[..genericTokenPos];
                    }

                    return typeName;
                }
                // 构建声明语句
                var invocationExpression = new InvocationExpression(typeName);
                foreach (Expression argument in typeArguments)
                {
                    invocationExpression.arguments.Add(argument);
                }
                Expression luaExpression = invocationExpression;
                transform?.ImportGenericTypeName(ref luaExpression, namedTypeSymbol);
                return luaExpression;
            }
        }

        /// <summary>
        /// 获取全部参数类型
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <param name="transform"></param>
        /// <returns></returns>
        private List<Expression> GetTypeArguments(INamedTypeSymbol typeSymbol, LuaSyntaxTreeBuilder transform) {
            List<Expression> typeArguments = new List<Expression>();
            FillExternalTypeArgument(typeArguments, typeSymbol, transform);
            FillTypeArguments(typeArguments, typeSymbol, transform);
            return typeArguments;
        }

        /// <summary>
        /// 填充外部参数类型
        /// </summary>
        /// <param name="typeArguments"></param>
        /// <param name="typeSymbol"></param>
        /// <param name="transform"></param>
        private void FillExternalTypeArgument(List<Expression> typeArguments, INamedTypeSymbol typeSymbol, LuaSyntaxTreeBuilder transform) {
            var externalType = typeSymbol.ContainingType;
            if (externalType != null) {
                FillExternalTypeArgument(typeArguments, externalType, transform);
                FillTypeArguments(typeArguments, externalType, transform);
            }
        }

        /// <summary>
        /// 填充参数类型
        /// </summary>
        /// <param name="typeArguments"></param>
        /// <param name="typeSymbol"></param>
        /// <param name="transform"></param>
        private void FillTypeArguments(List<Expression> typeArguments, INamedTypeSymbol typeSymbol, LuaSyntaxTreeBuilder transform) {
            if (typeSymbol.TypeArguments.Length > 0) {
                transform?.AddGenericTypeCounter();
                foreach (var typeArgument in typeSymbol.TypeArguments) {
                    if (typeArgument.Kind == SymbolKind.ErrorType) {
                        break;
                    }
                    var typeArgumentExpression = GetTypeName(typeArgument, transform);
                    typeArguments.Add(typeArgumentExpression);
                }
                transform?.SubGenericTypeCounter();
            }
        }

        /// <summary>
        /// 获取typename
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="transform"></param>
        /// <returns></returns>
        public IdentifierExpression GetTypeShortName(ISymbol symbol, LuaSyntaxTreeBuilder transform = null)
        {
            var typeSymbol = (INamedTypeSymbol) symbol.OriginalDefinition;
            string name = XmlMetaProvider.GetTypeShortName(typeSymbol, GetNamespaceMapName, GetTypeRefactorName, transform);
            string newName = metaProvider.GetTypeMapName(typeSymbol, name);
            // 如果xml中有，直接用xml的
            if (newName != null)
            {
                name = newName;
            }

            if (transform != null)
            {
                // 如果不会被导入
                if (transform.IsNoImportTypeName)
                {
                    // 判断如果开头不是system或者class，加上global
                    if (!name.StartsWith(LuaDefine.IdentifierName.System.name) &&
                        !name.StartsWith(LuaDefine.IdentifierName.Class.name))
                    {
                        name = LuaDefine.IdentifierName.Global.name + '.' + name;
                    }
                }
                else
                {
                    //如果会被导入，需要做typename检查
                    transform.ImportTypeName(ref name, (INamedTypeSymbol) symbol);
                }
            }

            return name;
        }
        
        private string GetTypeRefactorName(INamedTypeSymbol symbol) {
            if (_typeRefactorNames.TryGetValue(symbol, out var value))
            {
                return value;
            }
            return default;
        }

        /// <summary>
        /// 判断是否是常量枚举
        /// </summary>
        /// <param name="enumType"></param>
        /// <returns></returns>
        public bool IsConstantEnum(ITypeSymbol enumType)
        {
            if (enumType.TypeKind == TypeKind.Enum)
            {
                // 如果是常量并且不要求导出，就不导出。
                bool isNot = Setting.IsNotConstantForEnum && IsTypeEnableExport(enumType);
                return !isNot;
            }

            return false;
        }

        /// <summary>
        /// 判断是否要导出类型
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        bool IsTypeEnableExport(ITypeSymbol type)
        {
            bool isExport = true;
            if (type.TypeKind == TypeKind.Enum)
            {
                isExport = IsEnumExport(type.ToString());
            }

            if (_ignoreExportTypes.Contains(type))
            {
                isExport = false;
            }

            return isExport;
        }

        /// <summary>
        /// 判断是否要以枚举方式导出
        /// </summary>
        /// <param name="enumTypeSymbol"></param>
        /// <returns></returns>
        bool IsEnumExport(string enumTypeSymbol)
        {
            return Setting.IsExportEnumAll || _exportEnums.Contains(enumTypeSymbol);
        }

        /// <summary>
        /// 添加泛型导入依赖
        /// </summary>
        /// <param name="definition"></param>
        /// <param name="type"></param>
        /// <returns></returns>
        public bool AddGenericImportDepend(INamedTypeSymbol definition, INamedTypeSymbol type)
        {
            // 确保type不被重复添加
            if (type != null && !type.DeclaringSyntaxReferences.IsEmpty && !LuaTool.IsContainsType(definition, type) &&
                !LuaTool.IsDependExists(type, definition))
            {
                var set = _genericImportDepends.GetOrAdd(definition,
                    _ => new LuaTool.ConcurrentHashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default));
                return set.Add(type);
            }

            return false;
        }

        public void BuildCompilation()
        {
            List<string> codes = new List<string>();
            List<string> fileName = new List<string>();
            fileName.Add("Assets/Script/ToLua/Editor/HelloWorld.cs");
            foreach (var file in fileName)
            {
                codes.Add(File.ReadAllText(file));
            }

            string path = "";
            string[] cscArguments = null;
            var libs = GetAllLibs(new List<string>());
            var commandLineArguments = CSharpCommandLineParser.Default.Parse(
                (cscArguments ?? Array.Empty<string>()).Concat(new[] {"-define:__CSharpLua__"}), null, null);
            var parseOptions = commandLineArguments.ParseOptions.WithLanguageVersion(LanguageVersion.Preview)
                .WithDocumentationMode(DocumentationMode.Parse);
            List<SyntaxTree> trees = new List<SyntaxTree>();
            foreach (var c in codes)
            {
                trees.Add(CSharpSyntaxTree.ParseText(c, parseOptions, path));
            }

            List<PortableExecutableReference> loadedLib = new List<PortableExecutableReference>();
            foreach (var lib in libs)
            {
                loadedLib.Add(LoadLib(lib));
            }

            _compilation = CSharpCompilation.Create("name", trees, loadedLib);
            new CSharpToLuaSyntaxWalker(this);
        }

        public void AddImplicitInterfaceImplementation(ISymbol implementationMember, ISymbol interfaceMember)
        {
            if (!_implicitInterfaceImplementations.ContainsKey(implementationMember))
            {
                _implicitInterfaceImplementations.Add(implementationMember, new HashSet<ISymbol>());
            }

            if (_implicitInterfaceImplementations[implementationMember].Add(interfaceMember))
            {
                var containingType = implementationMember.ContainingType;
                Dictionary<ISymbol, ISymbol> map = default;
                if (map == null)
                {
                    map = new Dictionary<ISymbol, ISymbol>(SymbolEqualityComparer.Default);
                    _implicitInterfaceTypes.Add(containingType, map);
                }

                map.Add(interfaceMember, implementationMember);
            }
        }

        List<FileStream> LoadMeta(string[] metas)
        {
            List<FileStream> list = new List<FileStream>();
            foreach (var meta in metas)
            {
                var stream = new FileStream(meta, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                list.Add(stream);
            }

            return list;
        }

        private PortableExecutableReference LoadLib(string path)
        {
            Stream stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
            return MetadataReference.CreateFromStream(stream);
        }

        List<string> GetAllLibs(List<string> libs)
        {
            var allLibs = GetSystemLibs();
            // 遍历libs
            foreach (string lib in libs)
            {
                var path = lib;
                if (!path.EndsWith(".dll"))
                {
                    path += ".dll";
                }

                if (!File.Exists(path))
                {
                    Debug.Log("不存在的dll文件：" + path);
                    continue;
                }

                allLibs.Add(path);
            }

            return allLibs;
        }

        List<string> GetSystemLibs()
        {
            var systemLibs = new List<string>();
            var systemPath = Path.GetDirectoryName(typeof(object).Assembly.Location);
            var systemDlls = Directory.GetFiles(systemPath, "*.dll");
            foreach (var systemDll in systemDlls)
            {
                if (IsCorrectSystemDll(systemDll))
                    systemLibs.Add(systemDll);
            }

            return systemLibs;
        }

        private static bool IsCorrectSystemDll(string path)
        {
            try
            {
                Assembly.LoadFile(path);
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        public CSharpCompilation GetCompilation()
        {
            return _compilation;
        }

        public void AddType(INamedTypeSymbol typeSymbol)
        {
            _types.Add(typeSymbol);
            ProcessInherit(typeSymbol);
        }

        // 处理继承关系
        void ProcessInherit(INamedTypeSymbol typeSymbol)
        {
            if (typeSymbol.SpecialType != SpecialType.System_Object)
            {
                if (typeSymbol.BaseType != null)
                {
                    TryAddExtendSymbol(typeSymbol, typeSymbol);
                }
            }

            // AllInterfaces 包含所有接口，包括间接接口
            foreach (var interfaceType in typeSymbol.AllInterfaces)
            {
                TryAddExtendSymbol(interfaceType, typeSymbol);
            }
        }

        public void TryAddExtendSymbol(INamedTypeSymbol typeSymbol, INamedTypeSymbol child, bool isImplicit = false)
        {
            if (!typeSymbol.DeclaringSyntaxReferences.IsEmpty)
            {
                // 处理泛型
                if (typeSymbol.IsGenericType)
                {
                    typeSymbol = typeSymbol.OriginalDefinition;
                }

                if (isImplicit)
                {
                    if (!_implicitExtends.ContainsKey(typeSymbol))
                        _implicitExtends.Add(typeSymbol, new HashSet<INamedTypeSymbol>());
                    _implicitExtends[typeSymbol].Add(child);
                    return;
                }

                if (!_typeDependence.ContainsKey(typeSymbol))
                {
                    _typeDependence.Add(typeSymbol, new HashSet<INamedTypeSymbol>());
                }

                _typeDependence[typeSymbol].Add(child);
            }
        }

        public void AddTypesOfExtendSelf(INamedTypeSymbol symbol)
        {
            _typesOfExtendSelf.Add(symbol);
        }

        public void AddTypeRefactorNames(INamedTypeSymbol symbol, string name)
        {
            _typeRefactorNames.Add(symbol, name);
        }

        public void AddNamespaceRefactorNames(INamespaceSymbol symbol, string name)
        {
            _namespaceRefactorNames.Add(symbol, name);
        }
        
        public void AddExportEnum(ITypeSymbol enumType) {
            Contract.Assert(enumType.TypeKind == TypeKind.Enum);
            if (!enumType.DeclaringSyntaxReferences.IsEmpty) {
                _exportEnums.Add(enumType.ToString());
            }
        }
        
        /// <summary>
        /// 将symbol的成员名加入成员名列表
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns>symbol的name表达式</returns>
        public IdentifierNameExpression GetMemberName(ISymbol symbol) {
            // 拿到原始定义
            CheckOriginalDefinition(ref symbol);
            IdentifierNameExpression name = default;
            lock (_memberNames) {
                if (_memberNames.TryGetValue(symbol, out var v)) {
                    name = v;
                }
            }
            if (name == null) {
                lock(_memberNames) {
                    if (!_memberNames.TryGetValue(symbol, out var v)) {
                        // 处理symbol和其成员的名称
                        v = ProcessMemberName(symbol);
                        _memberNames.Add(symbol, v);
                    }

                    name = _memberNames[symbol];
                }
            }
            return name;
        }

        /// <summary>
        /// 处理symbol及其成员名
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns>symbol的name表达式</returns>
        SymbolExpression ProcessMemberName(ISymbol symbol) {
            // 得到处理后的symbol名称，并会记录和处理所有成员名
            var identifierName = InternalGetMemberName(symbol);
            // 判断symbol是否需要重构
            CheckMemberBadName(identifierName.name, symbol);
            return new SymbolExpression(identifierName);
        }
        
        /// <summary>
        /// 判断symbol是否需要重构
        /// </summary>
        /// <param name="originalString"></param>
        /// <param name="symbol"></param>
        private void CheckMemberBadName(string originalString, ISymbol symbol) {
            // 只有具有声明依赖的symbol才需要检查
            if (!symbol.DeclaringSyntaxReferences.IsEmpty) {
                bool isCheckNeedReserved = false;
                bool isCheckIllegalIdentifier = true;
                switch (symbol.Kind) {
                    // 字段和方法需要检查是否是保留字
                    case SymbolKind.Field:
                    case SymbolKind.Method:
                        isCheckNeedReserved = true;
                        break;

                    // 对于属性类型，需要判断是否是索引器
                    case SymbolKind.Property:
                        var propertySymbol = (IPropertySymbol)symbol;
                        if (propertySymbol.IsIndexer) {
                            isCheckIllegalIdentifier = false;
                        } else {
                            isCheckNeedReserved = true;
                        }
                        break;

                    // 对于事件类型，需要判断是否是事件字段
                    case SymbolKind.Event:
                        if (IsEventField((IEventSymbol)symbol)) {
                            isCheckNeedReserved = true;
                        }
                        break;
                }

                // 判断是否是保留字
                if (isCheckNeedReserved) {
                    if (LuaSyntaxNode.IsMethodReservedWord(originalString)) {
                        _refactorNames.Add(symbol);
                        isCheckIllegalIdentifier = false;
                    }
                }

                // 判断是否是非法标识符
                if (isCheckIllegalIdentifier) {
                    if (IsIdentifierIllegal(ref originalString)) {
                        _refactorNames.Add(symbol);
                        _memberIllegalNames.Add(symbol, originalString);
                    }
                }
            }
        }
        
        private static readonly Regex _identifierRegex = new(@"^[a-zA-Z_][a-zA-Z0-9_]*$", RegexOptions.Compiled);
        public static bool IsIdentifierIllegal(ref string identifierName) {
            if (!_identifierRegex.IsMatch(identifierName)) {
                identifierName = CSharpToLuaSyntaxWalker.EncodeToIdentifier(identifierName);
                return true;
            }
            return false;
        }
        
        bool IsEventField(IEventSymbol symbol) {
            return _isFieldEvents.GetOrAdd(symbol, symbol =>
                !_implicitInterfaceImplementations.ContainsKey(symbol) && IsEventFieldInternal(symbol));
        }
        
        bool IsEventFieldInternal(IEventSymbol symbol) {
            if (IsOverridable(symbol) || IsInterfaceImplementation(symbol)) {
                return false;
            }

            if (IsFromModuleOnly(symbol)) {
                return IsModuleAutoField(symbol);
            }

            if (IsFromAssembly(symbol)) {
                return false;
            }

            var node = LuaSyntaxTreeBuilder.GetDeclaringSyntaxNode(symbol);
            if (node != null) {
                return node.IsKind(SyntaxKind.VariableDeclarator);
            }

            return false;
        }
        
        private static bool IsModuleAutoField(ISymbol symbol) {
            var method = symbol.Kind == SymbolKind.Property ? ((IPropertySymbol)symbol).GetMethod : ((IEventSymbol)symbol).AddMethod;
            return method != null && HasCompilerGeneratedAttribute(method.GetAttributes());
        }
        
        public static bool HasCompilerGeneratedAttribute(ImmutableArray<AttributeData> attrs) {
            return attrs.Any(i => IsCompilerGeneratedAttribute(i.AttributeClass));
        }
        
        public static bool IsCompilerGeneratedAttribute(INamedTypeSymbol symbol) {
            return symbol.Name == "CompilerGeneratedAttribute" && IsRuntimeCompilerServices(symbol.ContainingNamespace);
        }
        
        public static bool IsRuntimeCompilerServices(INamespaceSymbol symbol) {
            return symbol.Name == "CompilerServices" && symbol.ContainingNamespace.Name == "Runtime" && symbol.ContainingNamespace.ContainingNamespace.Name == "System";
        }
        
        public static bool IsInterfaceImplementation<T>(T symbol) where T : ISymbol {
            if (!symbol.IsStatic) {
                var type = symbol.ContainingType;
                if (type != null) {
                    var interfaceSymbols = type.AllInterfaces.SelectMany(i => i.GetMembers().OfType<T>());
                    return interfaceSymbols.Any(i => symbol.Equals(type.FindImplementationForInterfaceMember(i)));
                }
            }
            return false;
        }
        
        public static bool IsOverridable(ISymbol symbol) {
            return !symbol.IsStatic && (symbol.IsAbstract || symbol.IsVirtual || symbol.IsOverride);
        }
        
        /// <summary>
        /// 获取成员名
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private IdentifierNameExpression InternalGetMemberName(ISymbol symbol) {
            // 方法
            if (symbol.Kind == SymbolKind.Method) {
                // 获取方法名
                string name = metaProvider.GetMethodMapName((IMethodSymbol)symbol);
                if (name != null) {
                    return name;
                }
            }
            // 属性
            else if (symbol.Kind == SymbolKind.Property) {
                // 获取属性名
                string name = metaProvider.GetPropertyMapName((IPropertySymbol)symbol);
                if (name != null) {
                    return name;
                }
            }

            // 如果不来自Lua模块
            if (!IsFromLuaModule(symbol)) {
                return GetSymbolBaseName(symbol);
            }

            // 如果是静态成员
            if (symbol.IsStatic) {
                if (symbol.ContainingType.IsStatic) {
                    return GetStaticClassMemberName(symbol);
                }
            }

            // 如果是重写符号，找到最上层的被重写符号
            while (symbol.IsOverride) {
                var overriddenSymbol = OverriddenSymbol(symbol);
                symbol = overriddenSymbol;
            }

            return GetAllTypeSameName(symbol);
        }
        
        /// <summary>
        /// 记录全部类型的同名成员
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns>symbol修饰后的name表达式</returns>
        /// <exception cref="InvalidOperationException"></exception>
        private IdentifierNameExpression GetAllTypeSameName(ISymbol symbol) {
            List<ISymbol> sameNameMembers = GetSameNameMembers(symbol);
            IdentifierNameExpression symbolExpression = null;
            int index = 0;
            // 处理每个同名成员
            foreach (ISymbol member in sameNameMembers) {
                // 判断是否是当前符号本身
                if (IsSameNameSymbol(member, symbol)) {
                    symbolExpression = GetSymbolBaseName(symbol);
                } else {
                    if (!_memberNames.ContainsKey(member)) {
                        IdentifierNameExpression identifierName = GetSymbolBaseName(member);
                        _memberNames.Add(member, new SymbolExpression(identifierName));
                    }
                }
                if (index > 0) {
                    // 记录原始定义到需要重构的符号列表
                    ISymbol refactorSymbol = member;
                    CheckOriginalDefinition(ref refactorSymbol);
                    _refactorNames.Add(refactorSymbol);
                }
                ++index;
            }
            if (symbolExpression == null) {
                throw new InvalidOperationException();
            }
            return symbolExpression;
        }
        
        /// <summary>
        /// 判断是否是同名符号
        /// </summary>
        /// <param name="member"></param>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private static bool IsSameNameSymbol(ISymbol member, ISymbol symbol) {
            // 完全相同
            if (SymbolEqualityComparer.Default.Equals(member, symbol)) {
                return true;
            }

            if (symbol.Kind == SymbolKind.Method) {
                var methodSymbol = (IMethodSymbol)symbol;
                // Partial修饰的方法
                if (methodSymbol.PartialDefinitionPart != null && SymbolEqualityComparer.Default.Equals(methodSymbol.PartialDefinitionPart, member)) {
                    return true;
                }
            }

            return false;
        }
        
        /// <summary>
        /// 获取symbol的同名成员
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private List<ISymbol> GetSameNameMembers(ISymbol symbol) {
            List<ISymbol> members = new List<ISymbol>();
            var names = GetSymbolNames(symbol);
            var rootType = symbol.ContainingType;
            var curTypeSymbol = rootType;
            // 获取同名成员，并不断向上查找基类
            while (true) {
                AddSimilarNameMembers(curTypeSymbol, names, members, !SymbolEqualityComparer.Default.Equals(rootType, curTypeSymbol));
                var baseTypeSymbol = curTypeSymbol.BaseType;
                if (baseTypeSymbol != null) {
                    curTypeSymbol = baseTypeSymbol;
                } else {
                    break;
                }
            }
            // 排序
            members.Sort(MemberSymbolComparison);
            return members;
        }
        
        /// <summary>
        /// symbol的比较器
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns></returns>
        /// <exception cref="Exception"></exception>
        private int MemberSymbolComparison(ISymbol a, ISymbol b) {
            // 相同符号
            if (SymbolEqualityComparer.Default.Equals(a, b)) {
                return 0;
            }

            // 比较是否来自Lua模块，不来自lua模块的优先级高
            bool isFromCodeOfA = IsFromLuaModule(a.ContainingType);
            bool isFromCodeOfB = IsFromLuaModule(b.ContainingType);

            if (!isFromCodeOfA) {
                if (!isFromCodeOfB) {
                    return 0;
                }

                return -1;
            }

            if (!isFromCodeOfB) {
                return 1;
            }

            // 比较接口实现的数量
            int countOfA = AllInterfaceImplementationsCount(a);
            int countOfB = AllInterfaceImplementationsCount(b);
            if (countOfA > 0 || countOfB > 0) {
                if (countOfA != countOfB) {
                    // 接口实现数量多的优先级高
                    return countOfA > countOfB ? -1 : 1;
                }

                // 比较都只有一个接口实现的情况
                if (countOfA == 1) {
                    var implementationOfA = InterfaceImplementations(a).First();
                    var implementationOfB = InterfaceImplementations(b).First();
                    if (SymbolEqualityComparer.Default.Equals(implementationOfA, implementationOfB)) {
                        throw new Exception($"{a} is conflict with {b}");
                    }
                    // 比较两个接口实现
                    if (MemberSymbolBoolComparison(implementationOfA, implementationOfB, i => !IsExplicitInterfaceImplementation(i), out int result)) {
                        return result;
                    }
                }

                return MemberSymbolCommonComparison(a, b);
            }

            if (MemberSymbolBoolComparison(a, b, i => i.IsAbstract, out var v)) {
                return v;
            }
            if (MemberSymbolBoolComparison(a, b, i => i.IsVirtual, out v)) {
                return v;
            }
            if (MemberSymbolBoolComparison(a, b, i => i.IsOverride, out v)) {
                return v;
            }

            return MemberSymbolCommonComparison(a, b);
        }
        
        private int MemberSymbolCommonComparison(ISymbol a, ISymbol b) {
            if (a.ContainingType.EQ(b.ContainingType)) {
                var type = a.ContainingType;
                var names = GetSymbolNames(a);
                List<ISymbol> members = new List<ISymbol>();
                AddSimilarNameMembers(type, names, members);
                int indexOfA = members.IndexOf(a);
                Contract.Assert(indexOfA != -1);
                int indexOfB = members.IndexOf(b);
                if (indexOfB == -1) {
                    var m = a.ContainingType.GetMembers();
                    indexOfA = m.IndexOf(a);
                    indexOfB = m.IndexOf(b);
                }
                Contract.Assert(indexOfA != indexOfB);
                return indexOfA.CompareTo(indexOfB);
            }

            bool isSubclassOf = IsSubclassOf(a.ContainingType, b.ContainingType);
            return isSubclassOf ? 1 : -1;
        }
        
        public static bool IsSubclassOf(ITypeSymbol child, ITypeSymbol parent) {
            if (parent.SpecialType == SpecialType.System_Object) {
                return true;
            }

            ITypeSymbol p = child;
            if (p.EQ(parent)) {
                return false;
            }

            while (p != null) {
                if (p.EQ(parent)) {
                    return true;
                }
                p = p.BaseType;
            }
            return false;
        }
        
        public static bool IsExplicitInterfaceImplementation(ISymbol symbol) {
            switch (symbol.Kind) {
                case SymbolKind.Property: {
                    IPropertySymbol property = (IPropertySymbol)symbol;
                    if (property.GetMethod != null) {
                        if (property.GetMethod.MethodKind == MethodKind.ExplicitInterfaceImplementation) {
                            return true;
                        }
                        if (property.SetMethod is {MethodKind: MethodKind.ExplicitInterfaceImplementation}) {
                            return true;
                        }
                    }
                    break;
                }
                case SymbolKind.Method: {
                    IMethodSymbol method = (IMethodSymbol)symbol;
                    if (method.MethodKind == MethodKind.ExplicitInterfaceImplementation) {
                        return true;
                    }
                    break;
                }
            }
            return false;
        }
        
        private bool MemberSymbolBoolComparison(ISymbol a, ISymbol b, Func<ISymbol, bool> boolFunc, out int v) {
            bool boolOfA = boolFunc(a);
            bool boolOfB = boolFunc(b);

            if (boolOfA) {
                if (boolOfB) {
                    v = MemberSymbolCommonComparison(a, b);
                } else {
                    v = -1;
                }
                return true;
            }

            if (b.IsAbstract) {
                v = 1;
                return true;
            }

            v = 0;
            return false;
        }
        
        /// <summary>
        /// 获取所有接口实现的数量
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private int AllInterfaceImplementationsCount(ISymbol symbol) {
            int count = 0;
            HashSet<ISymbol> implicitImplementations = default;
            if (_implicitInterfaceImplementations.TryGetValue(symbol, out var v)) {
                implicitImplementations = v;
            }
            if (implicitImplementations != null) {
                count = implicitImplementations.Count;
            }
            count += InterfaceImplementations(symbol).Count();
            return count;
        }
        
        public static IEnumerable<T> InterfaceImplementations<T>(T symbol) where T : ISymbol {
            if (!symbol.IsStatic) {
                var type = symbol.ContainingType;
                if (type != null) {
                    var interfaceSymbols = type.AllInterfaces.SelectMany(i => i.GetMembers().OfType<T>());
                    return interfaceSymbols.Where(i => symbol.Equals(type.FindImplementationForInterfaceMember(i)));
                }
            }
            return Array.Empty<T>();
        }
        
        /// <summary>
        /// 获取被重写的符号
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public static ISymbol OverriddenSymbol(ISymbol symbol) {
            switch (symbol.Kind) {
                case SymbolKind.Method: {
                    IMethodSymbol methodSymbol = (IMethodSymbol)symbol;
                    return methodSymbol.OverriddenMethod;
                }
                case SymbolKind.Property: {
                    IPropertySymbol propertySymbol = (IPropertySymbol)symbol;
                    return propertySymbol.OverriddenProperty;
                }
                case SymbolKind.Event: {
                    IEventSymbol eventSymbol = (IEventSymbol)symbol;
                    return eventSymbol.OverriddenEvent;
                }
            }
            return null;
        }
        
        /// <summary>
        /// 获取静态类的成员名
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        /// <exception cref="InvalidOperationException"></exception>
        private IdentifierNameExpression GetStaticClassMemberName(ISymbol symbol) {
            // 拿到全部同名成员
            var sameNameMembers = GetStaticClassSameNameMembers(symbol);
            IdentifierNameExpression symbolExpression = null;

            int index = 0;
            // 遍历每个同名成员
            foreach (ISymbol member in sameNameMembers) {
                // 找到一个合适的方法名
                IdentifierNameExpression identifierName = GetMethodNameFromIndex(symbol, index);
                // 判断是否是当前符号本身
                if (SymbolEqualityComparer.Default.Equals(member, symbol)) {
                    symbolExpression = identifierName;
                } else {
                    // 如果不是本身并且没有被记录过
                    if (!_memberNames.ContainsKey(member)) {
                        _memberNames.Add(member, new SymbolExpression(identifierName));
                    }
                }
                ++index;
            }

            if (symbolExpression == null) {
                throw new InvalidOperationException();
            }
            return symbolExpression;
        }
        
        /// <summary>
        /// 根据index获取方法名
        /// </summary>
        /// <param name="symbol"></param>
        /// <param name="index"></param>
        /// <returns></returns>
        private IdentifierNameExpression GetMethodNameFromIndex(ISymbol symbol, int index) {
            Contract.Assert(index != -1);
            if (index == 0) {
                return symbol.Name;
            }

            while (true) {
                string newName = symbol.Name + index;
                // 如果当前类名可用
                if (IsCurTypeNameEnable(symbol.ContainingType, newName)) {
                    TryAddNewUsedName(symbol.ContainingType, newName);
                    return newName;
                }
                ++index;
            }
        }
        
        /// <summary>
        /// 添加typename到已用列表
        /// </summary>
        /// <param name="type"></param>
        /// <param name="newName"></param>
        private void TryAddNewUsedName(INamedTypeSymbol type, string newName) {
            if (!_typeUsedNames.ContainsKey(type)) {
                _typeUsedNames[type] = new HashSet<string>();
            }

            _typeUsedNames[type].Add(newName);
        }
        
        /// <summary>
        /// 判断当前typename是否可用 不在已用列表并且不在成员列表
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <param name="newName"></param>
        /// <returns></returns>
        private bool IsCurTypeNameEnable(INamedTypeSymbol typeSymbol, string newName) {
            return !IsTypeNameUsed(typeSymbol, newName) && typeSymbol.GetMembers(newName).IsEmpty;
        }
        
        /// <summary>
        /// 判断是否已经被添加到已用列表
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <param name="newName"></param>
        /// <returns></returns>
        private bool IsTypeNameUsed(INamedTypeSymbol typeSymbol, string newName) {
            if (_typeUsedNames.TryGetValue(typeSymbol, out var name)) {
                if (name.Contains(newName)) {
                    return true;
                }
            }
            return false;
        }
        
        /// <summary>
        /// 获取静态方法同名成员
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private List<ISymbol> GetStaticClassSameNameMembers(ISymbol symbol) {
            List<ISymbol> members = new List<ISymbol>();
            var names = GetSymbolNames(symbol);
            AddSimilarNameMembers(symbol.ContainingType, names, members);
            return members;
        }
        
        /// <summary>
        /// 添加同名成员
        /// </summary>
        /// <param name="typeSymbol"></param>
        /// <param name="names"></param>
        /// <param name="outList">输出队列</param>
        /// <param name="isWithoutPrivate">是否加入私有成员</param>
        private void AddSimilarNameMembers(INamedTypeSymbol typeSymbol, List<string> names, List<ISymbol> outList, bool isWithoutPrivate = false) {
            foreach (var member in typeSymbol.GetMembers()) {
                if (member.IsOverride) {
                    continue;
                }

                if (!isWithoutPrivate || member.DeclaredAccessibility != Accessibility.Private) {
                    var memberNames = GetSymbolNames(member);
                    if (memberNames.Exists(names.Contains)) {
                        outList.Add(member);
                    }
                }
            }
        }
        
        /// <summary>
        /// 获取符号名
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private List<string> GetSymbolNames(ISymbol symbol) {
            List<string> names = new List<string>();
            switch (symbol.Kind) {
                case SymbolKind.Property:{
                    var propertySymbol = (IPropertySymbol)symbol;
                    if (IsPropertyField(propertySymbol)) {
                        names.Add(symbol.Name);
                    } else {
                        string baseName = GetSymbolBaseName(symbol);
                        if (propertySymbol.IsReadOnly) {
                            names.Add(LuaDefine.Tokens.Get + baseName);
                        } else if (propertySymbol.IsWriteOnly) {
                            names.Add(LuaDefine.Tokens.Set + baseName);
                        } else {
                            names.Add(LuaDefine.Tokens.Get + baseName);
                            names.Add(LuaDefine.Tokens.Set + baseName);
                        }
                    }

                    break;
                }
                case SymbolKind.Event:{
                    var eventSymbol = (IEventSymbol)symbol;
                    if (IsEventField(eventSymbol)) {
                        names.Add(symbol.Name);
                    } else {
                        string baseName = GetSymbolBaseName(symbol);
                        names.Add(LuaDefine.Tokens.Add + baseName);
                        names.Add(LuaDefine.Tokens.Remove + baseName);
                    }

                    break;
                }
                default:
                    names.Add(GetSymbolBaseName(symbol));
                    break;
            }
            return names;
        }
        
        bool IsPropertyField(IPropertySymbol symbol) {
            return _isFieldProperties.GetOrAdd(symbol, symbol => {
                bool? isMateField = metaProvider.IsPropertyField(symbol);
                return isMateField ?? (!_implicitInterfaceImplementations.ContainsKey(symbol) && IsPropertyFieldInternal(symbol));
            });
        }
        
        private bool IsPropertyFieldInternal(IPropertySymbol symbol) {
            if (IsOverridable(symbol) || IsInterfaceImplementation(symbol)) {
                return false;
            }

            if (IsFromModuleOnly(symbol)) {
                return IsModuleAutoField(symbol);
            }

            if (IsFromAssembly(symbol)) {
                return false;
            }

            return IsAutoProperty(symbol);
        }
        
        public static bool IsAutoProperty(IPropertySymbol symbol) {
            var node = LuaSyntaxTreeBuilder.GetDeclaringSyntaxNode(symbol);
            if (node != null) {
                switch (node.Kind()) {
                    case SyntaxKind.PropertyDeclaration: {
                        var property = (PropertyDeclarationSyntax)node;
                        bool hasGet = false;
                        bool hasSet = false;
                        if (property.AccessorList != null) {
                            foreach (var accessor in property.AccessorList.Accessors) {
                                if (accessor.Body != null || accessor.ExpressionBody != null) {
                                    if (accessor.IsKind(SyntaxKind.GetAccessorDeclaration)) {
                                        Contract.Assert(!hasGet);
                                        hasGet = true;
                                    } else {
                                        Contract.Assert(!hasSet);
                                        hasSet = true;
                                    }
                                }
                            }
                        } else {
                            Contract.Assert(!hasGet);
                            hasGet = true;
                        }
                        bool isField = !hasGet && !hasSet;
                        if (isField) {
                            if (HasCSharpLuaAttribute(property, DocumentStatement.AttributeFlags.NoField)) {
                                isField = false;
                            }
                        }
                        return isField;
                    }
                    case SyntaxKind.IndexerDeclaration: {
                        return false;
                    }
                    case SyntaxKind.AnonymousObjectMemberDeclarator: {
                        return true;
                    }
                    case SyntaxKind.Parameter: {
                        return true;
                    }
                    default: {
                        throw new InvalidOperationException();
                    }
                }
            }
            return false;
        }
        
        public static bool HasCSharpLuaAttribute(SyntaxNode node, DocumentStatement.AttributeFlags attribute) {
            var documentTrivia = node.GetLeadingTrivia().FirstOrDefault(i => i.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));
            if (documentTrivia != default) {
                string document = documentTrivia.ToString();
                if (document.Contains(DocumentStatement.ToString(attribute))) {
                    return true;
                }
            }
            return false;
        }
        
        public static bool IsFromAssembly(ISymbol symbol) {
            return symbol.DeclaringSyntaxReferences.IsEmpty;
        }
        
        /// <summary>
        /// 获取符号的基础名，主要额外获取了显式接口实现
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private string GetSymbolBaseName(ISymbol symbol) {
            switch (symbol.Kind) {
                case SymbolKind.Method: {
                    IMethodSymbol method = (IMethodSymbol)symbol;
                    string name = metaProvider.GetMethodMapName(method);
                    if (name != null) {
                        return name;
                    }
                    // 获取方法的显式接口实现
                    var implementation = method.ExplicitInterfaceImplementations.FirstOrDefault();
                    if (implementation != null) {
                        return implementation.Name;
                    }
                    break;
                }
                case SymbolKind.Property: {
                    IPropertySymbol property = (IPropertySymbol)symbol;
                    if (property.IsIndexer) {
                        return string.Empty;
                    }

                    // 获取属性的显式接口实现
                    var implementation = property.ExplicitInterfaceImplementations.FirstOrDefault();
                    if (implementation != null) {
                        return implementation.Name;
                    }
                    break;
                }
                case SymbolKind.Event: {
                    // 获取事件的显式接口实现
                    IEventSymbol eventSymbol = (IEventSymbol)symbol;
                    var implementation = eventSymbol.ExplicitInterfaceImplementations.FirstOrDefault();
                    if (implementation != null) {
                        return implementation.Name;
                    }
                    break;
                }
            }
            return symbol.Name;
        }
        
        /// <summary>
        /// 判断是否来自模块库
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        public bool IsFromLuaModule(ISymbol symbol) {
            return !symbol.DeclaringSyntaxReferences.IsEmpty || IsFromModuleOnly(symbol);
        }

        /// <summary>
        /// 判断是否来自模块库
        /// </summary>
        /// <param name="symbol"></param>
        /// <returns></returns>
        private bool IsFromModuleOnly(ISymbol symbol) {
            var luaModuleLibs = Setting.LuaModuleLibs;
            return luaModuleLibs != null && luaModuleLibs.Contains(symbol.ContainingAssembly.Name);
        }
        
        /// <summary>
        /// 检查原始定义
        /// </summary>
        /// <param name="symbol">返回原始定义</param>
        public static void CheckOriginalDefinition(ref ISymbol symbol) {
            if (symbol.Kind == SymbolKind.Method) {
                IMethodSymbol methodSymbol = (IMethodSymbol)symbol;
                CheckMethodDefinition(ref methodSymbol);
                if (!SymbolEqualityComparer.Default.Equals(methodSymbol, symbol)) {
                    symbol = methodSymbol;
                }
            } else {
                CheckSymbolDefinition(ref symbol);
            }
        }
        
        /// <summary>
        /// 获取方法的原始定义，主要特殊处理简化扩展方法
        /// </summary>
        /// <param name="symbol"></param>
        public static void CheckMethodDefinition(ref IMethodSymbol symbol) {
            if (symbol.IsExtensionMethod) {
                // 判断是否是简化扩展方法
                if (symbol.ReducedFrom != null && !SymbolEqualityComparer.Default.Equals(symbol.ReducedFrom, symbol)) {
                    symbol = symbol.ReducedFrom;
                } else {
                    CheckSymbolDefinition(ref symbol);
                }
            } else {
                // 如果不是扩展方法，直接按其他符号处理
                CheckSymbolDefinition(ref symbol);
            }
        }
        
        /// <summary>
        /// 获取符号原始定义
        /// </summary>
        /// <param name="symbol"></param>
        /// <typeparam name="T"></typeparam>
        private static void CheckSymbolDefinition<T>(ref T symbol) where T : class, ISymbol {
            var originalDefinition = (T)symbol.OriginalDefinition;
            if (originalDefinition != symbol) {
                symbol = originalDefinition;
            }
        }
    }
}