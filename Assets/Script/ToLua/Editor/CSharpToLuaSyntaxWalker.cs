using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor
{
    public class CSharpToLuaSyntaxWalker : CSharpSyntaxWalker
    {
        private LuaGenerator _generator; 
        HashSet<INamedTypeSymbol> _classType = new(SymbolEqualityComparer.Default);
        public CSharpToLuaSyntaxWalker(LuaGenerator generator)
        {
            _generator = generator;
            Init();
            PreProcess();
        }

        void Init()
        {
            foreach (SyntaxTree tree in _generator.GetCompilation().SyntaxTrees)
            {
                // 从根节点开始遍历得到classtype
                Visit(tree.GetRoot());
            }
        }
        
        // 预处理
        void PreProcess()
        {
            foreach (var type in _classType)
            {
                _generator.AddType(type);
                // 处理隐式接口实现
                ProcessImplicitInterfaceImplementation(type);
                ModifyTypeName(type);
            }
            
            ModifyNamespace();
        }
        
        // 修正命名空间
        void ModifyNamespace()
        {
            var all = _classType.SelectMany(i => GetAllNamespaces(i.ContainingNamespace)
                .Distinct(SymbolEqualityComparer.Default)
                .OfType<INamespaceSymbol>().ToArray());
            foreach (var symbol in all) {
                string name = symbol.Name;
                if (LuaSyntaxNode.IsReservedWord(name)) {
                    RefactorNamespaceName(all, symbol, symbol.Name, 1);
                } else {
                    if (IsIdentifierIllegal(ref name)) {
                        RefactorNamespaceName(all, symbol, name, 0);
                    }
                }
            }
        }

        private void RefactorNamespaceName(IEnumerable<INamespaceSymbol> all, INamespaceSymbol symbol, string symbolName, int i)
        {
            string newName = GetTypeOrNamespaceNewName(_classType, symbol, symbolName, i);
            _generator.AddNamespaceRefactorNames(symbol, newName);
        }

        // 获取所有命名空间
        private static IEnumerable<INamespaceSymbol> GetAllNamespaces(INamespaceSymbol symbol) {
            do {
                yield return symbol;
                symbol = symbol.ContainingNamespace;
            } while (!symbol.IsGlobalNamespace);
        }

        private void ModifyTypeName(INamedTypeSymbol type)
        {
            string name = type.Name;
            if (type.TypeParameters.IsEmpty) {
                // 修正lua保留字
                if (LuaSyntaxNode.IsReservedWord(name)) {
                    RefactorTypeName(type, name, 1);
                    return;
                }
            } else {
                string newName = name + '_' + type.TypeParameters.Length;
                // 修正重名
                if (CheckTypeNameExists(_classType, type, newName)) {
                    RefactorTypeName(type, name, 3);
                    return;
                }
            }

            // 修正非法标识符
            if (IsIdentifierIllegal(ref name)) {
                RefactorTypeName(type, name, 0);
            }
        }
        
        private static readonly Regex identifierRegex_ = new(@"^[a-zA-Z_][a-zA-Z0-9_]*$", RegexOptions.Compiled);
        
        // 检查类型名是否合法
        public static bool IsIdentifierIllegal(ref string identifierName) {
            if (!identifierRegex_.IsMatch(identifierName)) {
                // 编码不合法的标识符为合法
                identifierName = EncodeToIdentifier(identifierName);
                return true;
            }
            return false;
        }
        
        private static string EncodeToIdentifier(string name) {
            StringBuilder sb = new StringBuilder();
            foreach (char c in name) {
                if (c < 127) {
                    sb.Append(c);
                } else {
                    string base63 = ToBase63(c);
                    sb.Append(base63);
                }
            }
            if (char.IsNumber(sb[0])) {
                sb.Insert(0, '_');
            }
            return sb.ToString();
        }
        
        private static string ToBase63(int number) {
            const string kAlphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
            int basis = kAlphabet.Length;
            int n = number;
            StringBuilder sb = new StringBuilder();
            while (n > 0) {
                char ch = kAlphabet[n % basis];
                sb.Append(ch);
                n /= basis;
            }
            return sb.ToString();
        }
        
        private void RefactorTypeName(INamedTypeSymbol type, string name, int tag) {
            string newName = GetTypeOrNamespaceNewName(_classType, type, name, tag);
            _generator.AddTypeRefactorNames(type, newName);
        }
        
        private static string GetTypeOrNamespaceNewName(IEnumerable<ISymbol> allSymbols, ISymbol symbol, string name, int tag) {
            while (true) {
                string newName = GetNewIdentifierName(name, tag);
                if (!CheckTypeNameExists(allSymbols, symbol, newName)) {
                    return newName;
                }
            }
        }
        
        private static bool CheckTypeNameExists(IEnumerable<ISymbol> all, ISymbol type, string newName) {
            return all.Where(i => SymbolEqualityComparer.Default.Equals(i.ContainingNamespace, type.ContainingNamespace)).Any(i => i.Name == newName);
        }
        
        public static string GetNewIdentifierName(string name, int index) {
            return index switch {
                0 => name,
                1 => name + "_",
                2 => "_" + name,
                _ => name + (index - 2),
            };
        }
        
        private void ProcessImplicitInterfaceImplementation(INamedTypeSymbol type) {
            if (type.TypeKind == TypeKind.Class && !type.IsStatic) {
                foreach (var baseInterface in type.AllInterfaces) {
                    // 遍历非静态的接口成员
                    foreach (var interfaceMember in baseInterface.GetMembers().Where(i => !i.IsStatic)) {
                        var implementationMember = type.FindImplementationForInterfaceMember(interfaceMember);
                        if (implementationMember == null) {
                            continue;
                        }
              
                        // 检查是否是普通方法
                        if (implementationMember.Kind == SymbolKind.Method) {
                            var methodSymbol = (IMethodSymbol)implementationMember;
                            if (methodSymbol.MethodKind != MethodKind.Ordinary) {
                                continue;
                            }
                        }

                        var implementationType = implementationMember.ContainingType;
                        if (!SymbolEqualityComparer.Default.Equals(implementationMember, type)) {
                            if (!implementationType.AllInterfaces.Contains(baseInterface)) {
                                _generator.AddImplicitInterfaceImplementation(implementationMember, interfaceMember);
                                _generator.TryAddExtendSymbol(baseInterface, implementationType, true);
                            }
                        }
                    }
                }

                bool isExtenSelf = false;

                if (type.BaseType != null)
                {
                    isExtenSelf = CheckExtendSelf(type, type.BaseType);
                }

                foreach (INamedTypeSymbol symbol in type.AllInterfaces)
                {
                    if (isExtenSelf)
                    {
                        break;
                    }
                    isExtenSelf = CheckExtendSelf(type, symbol);
                }

                if (isExtenSelf)
                {
                    _generator.AddTypesOfExtendSelf(type);
                }
            }
        }

        public static bool CheckExtendSelf(INamedTypeSymbol typeSymbol, INamedTypeSymbol baseTypeSymbol) {
            if (baseTypeSymbol.IsGenericType) {
                foreach (var baseTypeArgument in baseTypeSymbol.TypeArguments) {
                    if (baseTypeSymbol.Kind != SymbolKind.TypeParameter) {
                        if (CheckSubClass(baseTypeArgument, typeSymbol)) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        public static bool CheckSubClass(ITypeSymbol baseTypeSymbol, ITypeSymbol childTypeSymbol)
        {
            if (baseTypeSymbol.SpecialType == SpecialType.System_Object) {
                return true;
            }

            ITypeSymbol p = childTypeSymbol;
            if (SymbolEqualityComparer.Default.Equals(baseTypeSymbol, p)) {
                return false;
            }

            while (p != null) {
                if (SymbolEqualityComparer.Default.Equals(baseTypeSymbol, p)) {
                    return true;
                }
                p = p.BaseType;
            }
            return false;
        }
        
        private void VisitBaseTypeDeclaration(BaseTypeDeclarationSyntax node, SyntaxList<MemberDeclarationSyntax> members) {
            var typeSymbol = _generator.GetCompilation().GetSemanticModel(node.SyntaxTree).GetDeclaredSymbol(node);
            _classType.Add(typeSymbol);

            var types = members.OfType<BaseTypeDeclarationSyntax>();
            foreach (var type in types) {
                type.Accept(this);
            }
        }
        
        public override void VisitClassDeclaration(ClassDeclarationSyntax node) {
            VisitBaseTypeDeclaration(node, node.Members);
        }

        public override void VisitStructDeclaration(StructDeclarationSyntax node) {
            VisitBaseTypeDeclaration(node, node.Members);
        }

        public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
        {
            var typeSymbol = _generator.GetCompilation().GetSemanticModel(node.SyntaxTree).GetDeclaredSymbol(node);
            _classType.Add(typeSymbol);
        }

        public override void VisitEnumDeclaration(EnumDeclarationSyntax node) {
            var typeSymbol = _generator.GetCompilation().GetSemanticModel(node.SyntaxTree).GetDeclaredSymbol(node);
            _classType.Add(typeSymbol);
        }

        public override void VisitRecordDeclaration(RecordDeclarationSyntax node) {
            var typeSymbol = _generator.GetCompilation().GetSemanticModel(node.SyntaxTree).GetDeclaredSymbol(node);
            _classType.Add(typeSymbol);
        }
    }
}