using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor {
    public class MethodInfo {
        public IMethodSymbol Symbol { get; }
        public IList<Exception> RefOrOutParameters { get; }
        public List<LuaDefine.IdentifierName> InliningReturnVars { get; set; }
        public bool IsInlining => InliningReturnVars != null;
        public bool HasInlineGoto;
        public bool HasYield;

        public MethodInfo(IMethodSymbol symbol) {
            Symbol = symbol;
            RefOrOutParameters = Array.Empty<Exception>();
        }

        public MethodInfo(IMethodSymbol symbol, IList<Exception> refOrOutParameters) {
            Symbol = symbol;
            RefOrOutParameters = refOrOutParameters;
        }
    }
}