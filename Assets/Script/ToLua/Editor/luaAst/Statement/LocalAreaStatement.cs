using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    public class LocalAreaStatement: Statement
    {
        public string keyword = LuaDefine.Keyword.Local;
        public List<IdentifierNameExpression> variables;
    }
}