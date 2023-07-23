using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst {
    public class TypeDeclarationStatement : FunctionStatement {
        public bool IsClassUsed { get; set; }
        private List<GenericUsingDeclare> _genericUsingDeclares = new();
        private FunctionExpression _function = new();
        
        /// <summary>
        /// 添加一个泛型的using声明
        /// </summary>
        /// <param name="invocationExpression"></param>
        /// <param name="name"></param>
        /// <param name="argumentTypeNames"></param>
        /// <param name="isFromCode"></param>
        /// <param name="genericUsingDeclare"></param>
        /// <returns></returns>
        public bool AddGenericImport(InvocationExpression invocationExpression, string name, List<string> argumentTypeNames, bool isFromCode, out GenericUsingDeclare genericUsingDeclare) {
            // 如果已经存在了，就不添加了
            if (_genericUsingDeclares.Exists(i => i.NewName == name)) {
                genericUsingDeclare = null;
                return true;
            }

            genericUsingDeclare = new GenericUsingDeclare {
                InvocationExpression = invocationExpression,
                ArgumentTypeNames = argumentTypeNames,
                NewName = name,
                IsFromCode = isFromCode
            };
            _genericUsingDeclares.Add(genericUsingDeclare);
            return true;
        }
        
        /// <summary>
        /// 添加全局参数
        /// </summary>
        public void AddGlobalParameter() {
            if (_function.parameters.Count == 1) {
                _function.parameters.Add(LuaDefine.IdentifierName.Global);
            }
        }
    }
}