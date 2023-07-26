using System.Collections.Generic;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor.luaAst
{
    public class Expression: LuaSyntaxNode
    {
        public static readonly Expression EmptyExpression = new EmptyExpression();
        
        public InvocationExpression Invocation()
        {
            return new InvocationExpression(this);
        }
        
        public InvocationExpression Invocation(IEnumerable<Expression> arguments) {
            return new(this, arguments);
        }

        public InvocationExpression Invocation(params Expression[] arguments) {
            return new(this, arguments);
        }

        /// <summary>
        /// 构造成员调用表达式
        /// </summary>
        /// <param name="name"></param>
        /// <param name="isColon"></param>
        /// <returns></returns>
        public MemberAccessExpression MemberAccess(Expression name, bool isColon = false)
        {
            return new MemberAccessExpression(this, name, isColon);
        }
        
        public static implicit operator Expression(string valueText) {
            IdentifierNameExpression identifierName = valueText;
            return identifierName;
        }
        
        public static implicit operator Expression(double number) {
            ValExpression numberLiteral = number;
            return numberLiteral;
        }
        
        public ParenthesizedExpression Parenthesized() {
            return new(this);
        }
        
        /// <summary>
        /// 构建左侧=右侧表达式
        /// </summary>
        /// <param name="right"></param>
        /// <returns></returns>
        public AssignmentExpression Assignment(Expression right) {
            return new(this, right);
        }
    }
}