using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    /// <summary>
    /// 函数调用表达式
    /// </summary>
    public class InvocationExpression: Expression
    {
        public ArgumentListSyntaxNode arguments; // 参数列表
        public Expression expression; // 函数表达式

        public InvocationExpression(Expression expression, Expression argument1, Expression argument2) : this(expression) {
            AddArgument(argument1);
            AddArgument(argument2);
        }

        public InvocationExpression(Expression expression, Expression argument1, Expression argument2, Expression argument3) : this(expression) {
            AddArgument(argument1);
            AddArgument(argument2);
            AddArgument(argument3);
        }
        
        public InvocationExpression(Expression expression)
        {
            this.expression = expression;
        }
        
        public InvocationExpression(Expression expression, Expression argument) : this(expression) {
            this.arguments.Arguments.Add(argument);
        }

        public InvocationExpression(Expression expression, params Expression[] arguments) : this(expression) {
            AddArguments(arguments);
        }
        
        public InvocationExpression(Expression expression, IEnumerable<Expression> arguments) : this(expression) {
            foreach (Expression argument in arguments) {
                this.arguments.Arguments.Add(argument);
            }
        }

        public void AddArgument(Expression argument)
        {
            arguments.Arguments.Add(argument);
        }
        
        public void AddArguments(IEnumerable<Expression> arguments) {
            this.arguments.AddArguments(arguments);
        }
    }
}