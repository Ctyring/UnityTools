namespace Script.ToLua.Editor.luaAst {
    public class PropertyOrEventIdentifierName: IdentifierNameExpression {
        public bool IsGetOrAdd { get; set; }
        public bool IsProperty { get; }
        public IdentifierNameExpression Name { get; }

        public PropertyOrEventIdentifierName(bool isProperty, IdentifierNameExpression name) : this(isProperty, true, name) {
        }

        public PropertyOrEventIdentifierName(bool isProperty, bool isGetOrAdd, IdentifierNameExpression name) : base(string.Empty) {
            IsProperty = isProperty;
            IsGetOrAdd = isGetOrAdd;
            Name = name;
        }

        public string PrefixToken {
            get {
                if (IsProperty) {
                    return IsGetOrAdd ? Tokens.Get : Tokens.Set;
                }

                return IsGetOrAdd ? Tokens.Add : Tokens.Remove;
            }
        }

        public PropertyOrEventIdentifierName GetClone() {
            return new(IsProperty, Name) { IsGetOrAdd = IsGetOrAdd };
        }
    }
}