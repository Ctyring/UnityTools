namespace Script.ToLua.Editor.luaAst
{
    public class IdentifierNameExpression: Expression
    {
        public string name;
        public IdentifierNameExpression(string name)
        {
            this.name = name;
        }
        
        public static implicit operator IdentifierNameExpression(string valueText) {
            return new(valueText);
        }
    }
}