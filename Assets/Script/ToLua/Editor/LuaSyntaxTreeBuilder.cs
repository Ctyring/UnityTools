using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor
{
    public class LuaSyntaxTreeBuilder : CSharpSyntaxVisitor<LuaSyntaxNode>
    {
        private List<Thunk> _thunks;
        private LuaGenerator _generator;
        private SemanticModel _semanticModel;
        public LuaSyntaxTreeBuilder(LuaGenerator generator, SemanticModel semanticModel) {
            _generator = generator;
            _semanticModel = semanticModel;
        }

        public void BuildLuaThunk(CompilationUnitSyntax node)
        {
            Thunk thunk = new Thunk(node.SyntaxTree.FilePath);
            _thunks.Add(thunk);
            
            ProcessTrivia(node, node.Members);
        }
        
        // 处理注释
        void ProcessTrivia(SyntaxNode root, SyntaxList<SyntaxNode> nodes)
        {
            List<SyntaxTrivia> trivias = null;
            foreach (SyntaxTrivia trivia in root.DescendantTrivia())
            {
                if(IsExportSyntaxTrivia(trivia, root))
                    trivias.Add(trivia);
            }
        }
        
        public bool IsExportSyntaxTrivia(SyntaxTrivia syntaxTrivia, SyntaxNode rootNode) {
            SyntaxKind kind = syntaxTrivia.Kind();
            switch (kind) {
                // 单行注释
                case SyntaxKind.SingleLineCommentTrivia:
                // 多行注释
                case SyntaxKind.MultiLineCommentTrivia:
                // 文档注释
                case SyntaxKind.SingleLineDocumentationCommentTrivia:
                // 禁用代码
                case SyntaxKind.DisabledTextTrivia:
                // region
                case SyntaxKind.RegionDirectiveTrivia:
                // endregion
                case SyntaxKind.EndRegionDirectiveTrivia: {
                    var span = rootNode.IsKind(SyntaxKind.CompilationUnit) ? rootNode.FullSpan : rootNode.Span;
                    return span.Contains(syntaxTrivia.Span);
                }
                default:
                    return false;
            }
        }
    }
}