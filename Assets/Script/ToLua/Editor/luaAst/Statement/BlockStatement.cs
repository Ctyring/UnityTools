using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    public class BlockStatement: Statement
    {
        public string openTag = LuaDefine.Keyword.Do;
        public string closeTag = LuaDefine.Keyword.End;
        public List<Statement> statements;
        
        // 本地变量声明
        LocalAreaStatement headVariables;

        void AddHeadVal(IdentifierNameExpression val)
        {
            if (headVariables == null)
            {
                headVariables = new LocalAreaStatement();
                // 加入到语句列表
                statements.Insert(0, headVariables);
            }
            headVariables.variables.Add(val);
        }

        public void AddStatement(Statement statement)
        {
            statements.Add(statement);
        }
    }
}