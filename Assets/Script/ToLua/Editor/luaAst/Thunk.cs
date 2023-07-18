using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    public class Thunk
    {
        private string _filePath;
        public List<Statement> _statements = new List<Statement>();
        public Thunk(string filePath)
        {
            _filePath = filePath;
            Init();
        }
        
        void Init()
        {
            // 添加System
            _statements.Add(new LocalVariableDeclaratorStatement(new IdentifierName("System"), new IdentifierName("System")));
        }
    }
}