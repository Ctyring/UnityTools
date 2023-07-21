using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using UnityEngine;

namespace Script.ToLua.Editor.luaAst
{
    public class BlockCommon : IComparable<BlockCommon>
    {
        public SyntaxTrivia syntaxTrivia;
        public CSharpSyntaxNode cSharpSyntaxNode;
        public FileLinePositionSpan linePositionSpan;

        // 用注释或者代码初始化块
        public BlockCommon(SyntaxTrivia trivia)
        {
            this.syntaxTrivia = trivia;
            linePositionSpan = trivia.SyntaxTree.GetLineSpan(trivia.Span);
        }

        public BlockCommon(CSharpSyntaxNode node)
        {
            cSharpSyntaxNode = node;
            linePositionSpan = node.SyntaxTree.GetLineSpan(node.Span);
        }
        
        // 排序整个文件的各个块
        public int CompareTo(BlockCommon other)
        {
            return linePositionSpan.StartLinePosition.CompareTo(other.linePositionSpan.StartLinePosition);
        }

        // 是否包含other
        public bool Contains(BlockCommon other)
        {
            var otherline = other.linePositionSpan;
            return otherline.StartLinePosition > linePositionSpan.StartLinePosition &&
                   otherline.EndLinePosition < linePositionSpan.EndLinePosition;
        }

        public LuaSyntaxNode Visit(LuaSyntaxTreeBuilder builder, out List<Statement> statements) {
            statements = null;
            const int kCommentCharCount = 2;
            if (cSharpSyntaxNode != null) {
                try {
                    var node = cSharpSyntaxNode.Accept(builder);
                    return node ?? throw new InvalidOperationException();
                } catch (Exception e) {
                    Debug.LogError("[BlockCommon::Visit] Error: " + e);
                }
            }

            string content = syntaxTrivia.ToString();
            switch (syntaxTrivia.Kind()) {
                // 短注释
                case SyntaxKind.SingleLineCommentTrivia: {
                    string commentContent = content[kCommentCharCount..];
                    return new ShortCommentStatement(commentContent);
                }
                // 多行注释
                case SyntaxKind.MultiLineCommentTrivia: {
                    // 切掉开头和结尾的--
                    string commentContent = content[kCommentCharCount..^kCommentCharCount];
                    commentContent = commentContent.Replace("\r\n", "\n").Replace("\r", "\n");
                    // 判断是否要插入lua代码
                    if (InsertLuaCodeTemplate(commentContent, out var codeStatements)) {
                        statements = codeStatements;
                        return null;
                    }
                    return new LongCommentStatement(commentContent, false);
                }
                // 空行
                case SyntaxKind.SingleLineDocumentationCommentTrivia:
                // 空行
                case SyntaxKind.DisabledTextTrivia: {
                    return new EmptyStatement();
                }
                // #region
                case SyntaxKind.RegionDirectiveTrivia:
                // #endregion
                case SyntaxKind.EndRegionDirectiveTrivia: {
                    // region直接按单行注释处理
                    return new ShortCommentStatement(content);
                }
                default:
                    throw new InvalidOperationException();
            }
        }
            
        // 在c#中插入lua代码 多行注释的形式
        public bool InsertLuaCodeTemplate(string commentContent, out List<Statement> statements) {
            statements = null;
            
            // 匹配开头的[
            char openBracket = LuaDefine.Tokens.OpenBracket[0];
            int index = commentContent.IndexOf(openBracket);
            if (index != -1) {
                char equals = LuaDefine.Tokens.Equals[0];
                int count = 0;
                ++index;
                // 匹配=
                while (commentContent[index] == equals) {
                    ++index;
                    ++count;
                }
                // 匹配最后的[
                if (commentContent[index] == openBracket) {
                    string closeToken = LuaDefine.Tokens.CloseBracket + new string(equals, count) + LuaDefine.Tokens.CloseBracket;
                    int begin = index + 1;
                    // 对称算出end
                    int end = commentContent.IndexOf(closeToken, begin, StringComparison.Ordinal);
                    if (end != -1) {
                        string codeString = commentContent[begin..end];
                        string[] lines = codeString.Split('\n');
                        var codeLines = new List<Statement>();
                        int indent = -1;
                        foreach (string line in lines) {
                            if (!string.IsNullOrWhiteSpace(line)) {
                                if (indent == -1) {
                                    indent = LuaTool.IndexOf(line, i => !char.IsWhiteSpace(i));
                                }
                                int space = LuaTool.IndexOf(line, i => !char.IsWhiteSpace(i));
                                string code = space >= indent && indent != -1 ? line[indent..] : line;
                                codeLines.Add((IdentifierNameExpression)code);
                            }
                        }
                        statements = codeLines;
                        return true;
                    }
                }
            }

            return false;
        }
        
        public BlankLineStatement ProcessBlankLine(ref int lastLine) {
            BlankLineStatement statement = null;
            if (lastLine != -1) {
                if (syntaxTrivia != default && syntaxTrivia.IsKind(SyntaxKind.DisabledTextTrivia)) {
                    ++lastLine;
                }
                int count = linePositionSpan.StartLinePosition.Line - lastLine - 1;
                if (count > 0) {
                    statement = new BlankLineStatement(count);
                }
            }

            lastLine = linePositionSpan.EndLinePosition.Line;
            return statement;
        }
    }
}