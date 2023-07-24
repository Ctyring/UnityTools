using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    /// <summary>
    /// 函数调用表达式
    /// </summary>
    public class InvocationExpression: Expression
    {
        public List<Expression> arguments; // 参数列表
        public Expression expression; // 函数表达式

        public InvocationExpression(Expression expression)
        {
            this.expression = expression;
        }
        
        public InvocationExpression(Expression expression, Expression argument) : this(expression) {
            this.arguments.Add(argument);
        }
        
        public InvocationExpression(Expression expression, Expression[] arguments) : this(expression) {
            foreach (Expression argument in arguments) {
                this.arguments.Add(argument);
            }
        }
        
        public InvocationExpression(Expression expression, IEnumerable<Expression> arguments) : this(expression) {
            foreach (Expression argument in arguments) {
                this.arguments.Add(argument);
            }
        }

        public void AddArgument(Expression argument)
        {
            arguments.Add(argument);
        }
    }
}