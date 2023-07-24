using System.Collections.Generic;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor.luaAst
{
    public class Expression: LuaSyntaxNode
    {
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
    }
}