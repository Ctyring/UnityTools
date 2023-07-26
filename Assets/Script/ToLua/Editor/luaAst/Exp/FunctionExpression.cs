using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    public class FunctionExpression: Expression
    {
        public List<IdentifierNameExpression> parameters;
        public int TempCount;

        public readonly BlockStatement body = new() {
            openTag = Tokens.Empty,
            closeTag = Keyword.End,
        };
        
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