using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    public class FunctionExpression: Expression
    {
        public List<IdentifierNameExpression> parameters;
        public BlockStatement body;

        public FunctionExpression()
        {
            body = new BlockStatement();
            parameters = new List<IdentifierNameExpression>();
        }
        
        public void AddParameter(IdentifierNameExpression parameter)
        {
            parameters.Add(parameter);
        }
        
        public void AddStatement(Statement statement)
        {
            body.AddStatement(statement);
        }
    }
}