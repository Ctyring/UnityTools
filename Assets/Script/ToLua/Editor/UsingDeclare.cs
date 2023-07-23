using System;
using System.Collections.Generic;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor
{
    public class GenericUsingDeclare : IComparable<GenericUsingDeclare> {
        public InvocationExpression InvocationExpression;
        public string NewName;
        public List<string> ArgumentTypeNames;
        public bool IsFromCode;
        public bool IsFromGlobal;

        public int CompareTo(GenericUsingDeclare other) {
            if (other.ArgumentTypeNames.Contains(NewName)) {
                return -1;
            }

            if (ArgumentTypeNames.Contains(other.NewName)) {
                return 1;
            }

            if (NewName.Length != other.NewName.Length) {
                return NewName.Length.CompareTo(other.NewName.Length);
            }

            return string.Compare(NewName, other.NewName, StringComparison.Ordinal);
        }
    }

    public class UsingDeclare : IComparable<UsingDeclare> {
        public string Prefix;
        public string NewPrefix;
        public bool IsFromCode;

        public int CompareTo(UsingDeclare other) {
            return string.Compare(Prefix, other.Prefix, StringComparison.Ordinal);
        }
    }
}