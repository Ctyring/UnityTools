using System;

namespace Script.ToLua.Editor.luaAst.Exp {
    public class ArrayTypeAdapterExpression : Expression{
        public Expression TypeExpression { get; }
        public ArrayRankSpecifier RankSpecifier { get; }

        public ArrayTypeAdapterExpression(Expression typeExpression, ArrayRankSpecifier rankSpecifier) {
            TypeExpression = typeExpression ?? throw new ArgumentNullException(nameof(typeExpression));
            RankSpecifier = rankSpecifier ?? throw new ArgumentNullException(nameof(rankSpecifier));
        }

        public bool IsSimpleArray {
            get {
                return RankSpecifier.Rank == 1;
            }
        }
    }
}