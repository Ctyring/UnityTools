using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.Contracts;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
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
        bool IsConstantEnum(ITypeSymbol enumType)
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
        
        public IdentifierNameExpression GetMemberName(ISymbol symbol) {
            CheckOriginalDefinition(ref symbol);
            IdentifierNameExpression name = default;
            lock (_memberNames) {
                if (_memberNames.TryGetValue(symbol, out var v)) {
                    name = v;
                }
            }
            if (name == null) {
                lock(_memberNames) {
                    name = _memberNames.GetOrAdd(symbol, symbol => {
                        var identifierName = InternalGetMemberName(symbol);
                        CheckMemberBadName(identifierName.ValueText, symbol);
                        return new LuaSymbolNameSyntax(identifierName);
                    });
                }
            }
            return name;
        }
        
        private IdentifierNameExpression InternalGetMemberName(ISymbol symbol) {
            if (symbol.Kind == SymbolKind.Method) {
                string name = metaProvider.GetMethodMapName((IMethodSymbol)symbol);
                if (name != null) {
                    return name;
                }
            } else if (symbol.Kind == SymbolKind.Property) {
                string name = metaProvider.GetPropertyMapName((IPropertySymbol)symbol);
                if (name != null) {
                    return name;
                }
            }

            if (!IsFromLuaModule(symbol)) {
                return GetSymbolBaseName(symbol);
            }

            if (symbol.IsStatic) {
                if (symbol.ContainingType.IsStatic) {
                    return GetStaticClassMemberName(symbol);
                }
            }

            while (symbol.IsOverride) {
                var overriddenSymbol = symbol.OverriddenSymbol();
                symbol = overriddenSymbol;
            }

            return GetAllTypeSameName(symbol);
        }
        
        private IdentifierNameExpression GetStaticClassMemberName(ISymbol symbol) {
            var sameNameMembers = GetStaticClassSameNameMembers(symbol);
            IdentifierNameExpression symbolExpression = null;

            int index = 0;
            foreach (ISymbol member in sameNameMembers) {
                IdentifierNameExpression identifierName = GetMethodNameFromIndex(symbol, index);
                if (member.EQ(symbol)) {
                    symbolExpression = identifierName;
                } else {
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
        
        private string GetSymbolBaseName(ISymbol symbol) {
            switch (symbol.Kind) {
                case SymbolKind.Method: {
                    IMethodSymbol method = (IMethodSymbol)symbol;
                    string name = metaProvider.GetMethodMapName(method);
                    if (name != null) {
                        return name;
                    }
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

                    var implementation = property.ExplicitInterfaceImplementations.FirstOrDefault();
                    if (implementation != null) {
                        return implementation.Name;
                    }
                    break;
                }
                case SymbolKind.Event: {
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
        
        internal bool IsFromLuaModule(ISymbol symbol) {
            return !symbol.DeclaringSyntaxReferences.IsEmpty || IsFromModuleOnly(symbol);
        }

        private bool IsFromModuleOnly(ISymbol symbol) {
            var luaModuleLibs = Setting.LuaModuleLibs;
            return luaModuleLibs != null && luaModuleLibs.Contains(symbol.ContainingAssembly.Name);
        }
        
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
        
        public static void CheckMethodDefinition(ref IMethodSymbol symbol) {
            if (symbol.IsExtensionMethod) {
                if (symbol.ReducedFrom != null && !SymbolEqualityComparer.Default.Equals(symbol.ReducedFrom, symbol)) {
                    symbol = symbol.ReducedFrom;
                } else {
                    CheckSymbolDefinition(ref symbol);
                }
            } else {
                CheckSymbolDefinition(ref symbol);
            }
        }
        
        private static void CheckSymbolDefinition<T>(ref T symbol) where T : class, ISymbol {
            var originalDefinition = (T)symbol.OriginalDefinition;
            if (originalDefinition != symbol) {
                symbol = originalDefinition;
            }
        }
    }
}