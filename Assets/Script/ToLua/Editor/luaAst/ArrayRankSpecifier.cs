using System.Collections.Generic;

namespace Script.ToLua.Editor.luaAst {
    public class ArrayRankSpecifier : LuaSyntaxNode{
        public int Rank { get; }
        public readonly List<Expression> Sizes = new();

        public ArrayRankSpecifier(int rank) {
            Rank = rank;
        }
    }
}