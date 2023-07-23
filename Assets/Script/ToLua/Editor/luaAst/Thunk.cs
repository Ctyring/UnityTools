using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst
{
    public class Thunk
    {
        private string _filePath;
        public List<Statement> _statements = new List<Statement>();
        public List<GenericUsingDeclare> genericUsingDeclares = new();
        public List<UsingDeclare> usingDeclares = new();
        public Thunk(string filePath)
        {
            _filePath = filePath;
            Init();
        }
        
        void Init()
        {
            // 添加System
            _statements.Add(new LocalVariableDeclaratorStatement(new IdentifierNameExpression("System"), new IdentifierNameExpression("System")));
        }
        
        public void AddStatement(Statement statement)
        {
            _statements.Add(statement);
        }
        
        /// <summary>
        /// 判断是否有声明冲突
        /// </summary>
        /// <param name="generic"></param>
        /// <returns></returns>
        public bool IsUsingDeclareConflict(InvocationExpression generic) {
            if (generic.expression is IdentifierNameExpression identifier) {
                int pos = identifier.name.IndexOf('.');
                if (pos != -1) {
                    string prefix = identifier.name[..pos];
                    return usingDeclares.Exists(i => i.NewPrefix == prefix);
                }
            }
            return false;
        }
    }
}