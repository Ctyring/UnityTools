using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Script.ToLua.Editor.CSharpToLuaHelper
{
    public class LocalValueWalker: CSharpSyntaxWalker
    {
        private sealed class FoundException : Exception {
        }

        protected void Found() {
            throw new FoundException();
        }

        public bool Find(SyntaxNode root) {
            try {
                Visit(root);
            } catch (FoundException) {
                return true;
            }
            return false;
        }
        
        private readonly string name_;

        public LocalValueWalker(string name) {
            name_ = name;
        }

        public override void VisitParameter(ParameterSyntax node) {
            if (node.Identifier.ValueText == name_) {
                Found();
            }
        }

        public override void VisitVariableDeclarator(VariableDeclaratorSyntax node) {
            if (node.Identifier.ValueText == name_) {
                Found();
            }
        }
    }
}