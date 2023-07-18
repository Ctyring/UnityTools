namespace Script.ToLua.Editor.luaAst
{
    public class IdentifierName: Expression
    {
        private string _name;
        public IdentifierName(string name)
        {
            this._name = name;
        }
    }
}