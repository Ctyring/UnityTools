
namespace Script.ToLua.Editor.luaAst
{
    /// <summary>
    /// 成员访问类
    /// </summary>
    public class MemberAccessExpression: Expression
    {
        public Expression expression; // 表达式
        public Expression name; // 访问的成员名
        public string operatorStr;

        public MemberAccessExpression(Expression expression, Expression name, bool isColon)
        {
            this.expression = expression;
            this.name = name;
            this.operatorStr = isColon ? LuaDefine.Tokens.ObjectColon : LuaDefine.Tokens.Dot;
        }

        void UpdateExpression(Expression expression)
        {
            this.expression = expression;
        }
    }
}