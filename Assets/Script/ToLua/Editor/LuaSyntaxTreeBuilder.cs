using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor
{
    public partial class LuaSyntaxTreeBuilder : CSharpSyntaxVisitor<LuaSyntaxNode>
    {
        
        private LuaGenerator _generator;
        private SemanticModel _semanticModel;
        private int _genericTypeCounter;
        
        public bool IsNoneGenericTypeCounter => _genericTypeCounter == 0;
        public void AddGenericTypeCounter() => ++_genericTypeCounter;
        public void SubGenericTypeCounter() => --_genericTypeCounter;
        
        private Stack<Thunk> _thunks = new Stack<Thunk>();
        private readonly Stack<TypeDeclarationInfo> _typeDeclarations = new();
        private readonly Stack<FunctionExpression> _functions = new();
        private readonly Stack<MethodInfo> _methodInfos = new();
        private readonly Stack<BlockStatement> _blocks = new();
        private readonly Stack<LuaIfStatementSyntax> ifStatements_ = new();
        private readonly Stack<LuaSwitchAdapterStatementSyntax> switches_ = new();
        private int _metadataTypeNameCounter;
        public bool IsMetadataTypeName => _metadataTypeNameCounter > 0;
        
        private Thunk CurrentThunk {
            get {
                return _thunks.Peek();
            }
        }

        private TypeDeclarationStatement CurType {
            get {
                return _typeDeclarations.Peek().TypeDeclaration;
            }
        }

        private INamedTypeSymbol CurTypeSymbol {
            get {
                return _typeDeclarations.Peek().TypeSymbol;
            }
        }

        internal TypeDeclarationInfo CurTypeDeclaration {
            get {
                return _thunks.Count > 0 ? _typeDeclarations.Peek() : null;
            }
        }

        private FunctionExpression CurFunction {
            get {
                return _functions.Peek();
            }
        }

        private FunctionExpression CurFunctionOrNull {
            get {
                return _functions.Count > 0 ? _functions.Peek() : null;
            }
        }

        private MethodInfo CurMethodInfoOrNull {
            get {
                return _methodInfos.Count > 0 ? _methodInfos.Peek() : null;
            }
        }
        
        private BlockStatement CurBlock {
            get {
                return _blocks.Peek();
            }
        }
        
        public LuaSyntaxTreeBuilder(LuaGenerator generator, SemanticModel semanticModel) {
            _generator = generator;
            _semanticModel = semanticModel;
        }

        public Thunk BuildLuaThunk(CompilationUnitSyntax node)
        {
            Thunk thunk = new Thunk(node.SyntaxTree.FilePath);
            _thunks.Push(thunk);
            
            var commons = Process(node, node.Members);
            foreach (Statement statement in commons) {
                if (statement is TypeDeclarationStatement typeDeclaration) {
                    var namespaceStatement = new FunctionStatement();
                    namespaceStatement.UpdateIdentifiers("", LuaDefine.IdentifierName.System, LuaDefine.IdentifierName.Namespace, LuaDefine.IdentifierName.Namespace);
                    namespaceStatement.function.body.AddStatement(statement);
                    thunk.AddStatement(namespaceStatement);
                }
                else {
                    thunk.AddStatement(statement);
                }
            }

            var attributes = ProcessAttributes(node.AttributeLists);
            _generator.assemblyAttributes.AddRange(attributes);
            _thunks.Pop();

            return thunk;
        }

        public List<Expression> ProcessAttributes(SyntaxList<AttributeListSyntax> attributeLists) {
            var expressions = new List<Expression>();
            var attributes = attributeLists.SelectMany(i => i.Attributes);
            foreach (AttributeSyntax attribute in attributes) {
                var expression = (Expression)attribute.Accept(this);
                if (expression != null) {
                    expressions.Add(expression);
                }
            }

            return expressions;
        }

        // 处理注释
        List<Statement> Process(SyntaxNode root, IEnumerable<CSharpSyntaxNode> nodes, bool processBlank = true)
        {
            List<SyntaxTrivia> trivias = new List<SyntaxTrivia>();
            foreach (SyntaxTrivia trivia in root.DescendantTrivia())
            {
                if(IsExportSyntaxTrivia(trivia, root))
                    trivias.Add(trivia);
            }

            List<BlockCommon> triList = new List<BlockCommon>();
            List<BlockCommon> nodeList = new List<BlockCommon>();

            foreach (var trivia in trivias)
            {
                triList.Add(new BlockCommon(trivia));
            }

            foreach (var syntaxNode in nodes)
            {
                nodeList.Add(new BlockCommon(syntaxNode));
            }

            bool tag = false;
            foreach (var blockCommon in triList)
            {
                if (!nodeList.Any(i => i.Contains(blockCommon)))
                {
                    nodeList.Add(blockCommon);
                    tag = true;
                }
            }

            if (tag)
            {
                nodeList.Sort();
            }

            List<Statement> statements = new List<Statement>();

            int lastLine = -1;
            foreach (var blockCommon in nodeList)
            {
                // 是否处理空白行
                if (processBlank)
                {
                    var blankLine = blockCommon.ProcessBlankLine(ref lastLine);
                    if (blankLine != null)
                    {
                        statements.Add(blankLine);
                    }
                }

                var blockNode = blockCommon.Visit(this, out var blockStatements);
                if (blockNode != null) {
                    statements.Add((Statement)blockNode);
                }

                if (blockStatements != null) {
                    statements.AddRange(blockStatements);
                }
            }
            
            return statements;
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

        public override LuaSyntaxNode VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
        {
            return BuildNamespaceDeclaration(node);
        }

        public FunctionStatement BuildNamespaceDeclaration(BaseNamespaceDeclarationSyntax node)
        {
            var symbol = (INamespaceSymbol)_semanticModel.GetDeclaredSymbol(node);
            // 检查是否是嵌套空间
            bool isNested = node.Parent.IsKind(SyntaxKind.NamespaceDeclaration);
            string name = _generator.GetNamespaceDefineName(symbol, node);
            var namespaceStatement = new FunctionStatement();
            namespaceStatement.UpdateIdentifiers(name, isNested ? LuaDefine.IdentifierName.Namespace : LuaDefine.IdentifierName.System, LuaDefine.IdentifierName.Namespace, LuaDefine.IdentifierName.Namespace);
            var statements = 
        }
    }
}