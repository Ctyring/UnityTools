namespace Script.ToLua.Editor.luaAst {
    public class IdentifierExpression: ValExpression {
        public IdentifierNameExpression identifierName;
        public IdentifierExpression(string value) : base(value, new IdentifierNameExpression(value)) {
            identifierName = new IdentifierNameExpression(value);
        }
        
        public static implicit operator IdentifierExpression(string valueText) {
            return new(valueText);
        }
        
        public static readonly IdentifierExpression Nil = new(LuaDefine.Keyword.Nil);
        public static readonly IdentifierExpression True = new(LuaDefine.Keyword.True);
        public static readonly IdentifierExpression False = new(LuaDefine.Keyword.False);
    }
}