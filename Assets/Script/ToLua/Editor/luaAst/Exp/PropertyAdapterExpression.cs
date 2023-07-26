using System.Diagnostics.Contracts;

namespace Script.ToLua.Editor.luaAst {
    public class PropertyAdapterExpression: Expression {
        public Expression Expression { get; private set; }
        public string OperatorToken { get; private set; }
        public PropertyOrEventIdentifierName Name { get; }
        public readonly ArgumentListSyntaxNode ArgumentList = new();

        public PropertyAdapterExpression(PropertyOrEventIdentifierName name) {
            Name = name;
        }

        public PropertyAdapterExpression(Expression expression, PropertyOrEventIdentifierName name, bool isObjectColon) {
            Update(expression, isObjectColon);
            Name = name;
        }

        public void Update(Expression expression, bool isObjectColon) {
            Contract.Assert(Expression == null);
            Expression = expression;
            OperatorToken = isObjectColon ? LuaDefine.Tokens.ObjectColon : LuaDefine.Tokens.Dot;
        }

        public void Update(Expression expression) {
            Expression = expression;
        }

        public bool IsGetOrAdd {
            set {
                Name.IsGetOrAdd = value;
            }
            get {
                return Name.IsGetOrAdd;
            }
        }

        public bool IsProperty {
            get {
                return Name.IsProperty;
            }
        }

        public bool IsObjectColon {
            get {
                return OperatorToken == LuaDefine.Tokens.ObjectColon;
            }
        }

        /// <summary>
        /// 克隆
        /// </summary>
        /// <returns></returns>
        public PropertyAdapterExpression GetClone() {
            PropertyAdapterExpression clone = new PropertyAdapterExpression(Name.GetClone()) {
                Expression = Expression,
                OperatorToken = OperatorToken,
            };
            clone.ArgumentList.Arguments.AddRange(ArgumentList.Arguments);
            return clone;
        }

        /// <summary>
        /// 克隆
        /// </summary>
        /// <returns></returns>
        public PropertyAdapterExpression GetCloneOfGet() {
            PropertyAdapterExpression clone = GetClone();
            clone.IsGetOrAdd = true;
            IsGetOrAdd = false;
            return clone;
        }
    }
}