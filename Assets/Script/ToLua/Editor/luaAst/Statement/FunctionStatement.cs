using Unity.VisualScripting.Dependencies.NCalc;

namespace Script.ToLua.Editor.luaAst
{
    public class FunctionStatement: Statement
    {
        public ExpressionStatement statement;
        public FunctionExpression function;

        public void UpdateIdentifiers(IdentifierNameExpression name, IdentifierNameExpression target,
            IdentifierNameExpression memberName, IdentifierNameExpression parameter = null)
        {
            var invoke = target.MemberAccess(memberName).Invocation();
            invoke.AddArgument(new StringExpression(name));
            invoke.AddArgument(function);
            if (parameter != null)
            {
                invoke.AddArgument(parameter);
            }

            statement = new ExpressionStatement(invoke);
        }
    }
}