using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
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
        Dictionary<INamedTypeSymbol, HashSet<INamedTypeSymbol>> _typeDependence = new Dictionary<INamedTypeSymbol, HashSet<INamedTypeSymbol>>(); // 类型依赖
        Dictionary<INamedTypeSymbol, HashSet<INamedTypeSymbol>> _implicitExtends = new(SymbolEqualityComparer.Default);
        private Dictionary<ISymbol, HashSet<ISymbol>> _implicitInterfaceImplementations = new(SymbolEqualityComparer.Default);
        private Dictionary<INamedTypeSymbol, Dictionary<ISymbol, ISymbol>> _implicitInterfaceTypes = new(SymbolEqualityComparer.Default);
        private HashSet<INamedTypeSymbol> _typesOfExtendSelf = new(SymbolEqualityComparer.Default);
        private Dictionary<INamedTypeSymbol, string> _typeRefactorNames = new(SymbolEqualityComparer.Default);
        Dictionary<INamespaceSymbol, string> _namespaceRefactorNames = new(SymbolEqualityComparer.Default);
        public XmlMetaProvider metaProvider;
        private string metas = "";
        public ImmutableList<Expression> assemblyAttributes = ImmutableList<Expression>.Empty;

        public void Init()
        {
            BuildCompilation();
            metaProvider = new XmlMetaProvider(LoadMeta(LuaTool.Split(metas)));
        }
        
        public void CreateLua()
        {
            foreach (SyntaxTree tree in _compilation.SyntaxTrees) {
                CreateLuaCompilationUnitSyntax(tree);
            }
        }

        Thunk CreateLuaCompilationUnitSyntax(SyntaxTree tree)
        {
            var semanticModel = _compilation.GetSemanticModel(tree);
            var syntax = (CompilationUnitSyntax)tree.GetRoot();
            LuaSyntaxTreeBuilder builder = new LuaSyntaxTreeBuilder(this, semanticModel);
            return builder.BuildLuaThunk(syntax);
        }
        
        private string GetNamespaceNames(List<INamespaceSymbol> symbols)
        {
            List<string> names = new List<string>();
            foreach (INamespaceSymbol symbol in symbols)
            {
                string name = symbol.Name;
                if(_namespaceRefactorNames.TryGetValue(symbol, out string refactorName))
                    name = refactorName;
                names.Add(name);
            }
            return string.Join(".", names);
        }

        private string GetNamespaceMapName(INamespaceSymbol symbol, string original) {
            if (!symbol.DeclaringSyntaxReferences.IsEmpty) {
                return GetNamespaceNames(GetAllNamespaces(symbol));
            }

            return metaProvider.GetNamespaceMapName(symbol, original);
        }
        
        public static IEnumerable<INamespaceSymbol> InternalGetAllNamespaces(INamespaceSymbol symbol) {
            do {
                yield return symbol;
                symbol = symbol.ContainingNamespace;
            } while (!symbol.IsGlobalNamespace);
        }

        public static List<INamespaceSymbol> GetAllNamespaces(INamespaceSymbol symbol) {
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

            var namespaces = new List<INamespaceSymbol>() { symbol };
            do
            {
                // 递归获取命名空间
                symbol = symbol.ContainingNamespace;
                namespaces.Add(symbol);
                List<INamespaceSymbol> symbols = namespaces;
                symbols.Reverse();
                string symbolsName = string.Join(".", symbols.Select(i => i.Name));
                if (symbolsName == original) {
                    return GetNamespaceNames(symbols);
                }
            } while (!symbol.IsGlobalNamespace);

            throw new InvalidOperationException();
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
            var commandLineArguments = CSharpCommandLineParser.Default.Parse((cscArguments ?? Array.Empty<string>()).Concat(new [] { "-define:__CSharpLua__" }), null, null);
            var parseOptions = commandLineArguments.ParseOptions.WithLanguageVersion(LanguageVersion.Preview).WithDocumentationMode(DocumentationMode.Parse);
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
        
        public void AddImplicitInterfaceImplementation(ISymbol implementationMember, ISymbol interfaceMember) {
            if (!_implicitInterfaceImplementations.ContainsKey(implementationMember))
            {
                _implicitInterfaceImplementations.Add(implementationMember, new HashSet<ISymbol>());
            }
            if (_implicitInterfaceImplementations[implementationMember].Add(interfaceMember)) {
                var containingType = implementationMember.ContainingType; 
                Dictionary<ISymbol, ISymbol> map = default;
                if (map == null) {
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
                if(IsCorrectSystemDll(systemDll))
                    systemLibs.Add(systemDll);
            }
            return systemLibs;
        }
        
        private static bool IsCorrectSystemDll(string path) {
            try {
                Assembly.LoadFile(path);
                return true;
            } catch (Exception) {
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
                    if(!_implicitExtends.ContainsKey(typeSymbol))
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
    }
}