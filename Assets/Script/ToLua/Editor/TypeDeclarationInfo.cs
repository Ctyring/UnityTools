using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor {
    public class TypeDeclarationInfo {
        public INamedTypeSymbol TypeSymbol { get; }
        public TypeDeclarationStatement TypeDeclaration { get; }

        public TypeDeclarationInfo(INamedTypeSymbol typeSymbol, TypeDeclarationStatement luaTypeDeclarationSyntax) {
            TypeSymbol = typeSymbol;
            TypeDeclaration = luaTypeDeclarationSyntax;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="getNameTypeSymbol"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        public bool CheckTypeName(INamedTypeSymbol getNameTypeSymbol, out IdentifierNameExpression name) {
            if (SymbolEqualityComparer.Default.Equals(getNameTypeSymbol, TypeSymbol)) {
                TypeDeclaration.IsClassUsed = true;
                name = LuaDefine.IdentifierName.Class;
                return true;
            }
            name = null;
            return false;
        }
    }
}