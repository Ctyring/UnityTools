/*
Copyright 2017 YANG Huan (sy.yanghuan@gmail.com).

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

using System;
using System.Collections.Generic;
using System.Linq;
using Script.ToLua.Editor.luaAst;
using static Script.ToLua.Editor.luaAst.LuaDefine.Tokens;

namespace CSharpLua.LuaAst {
  public sealed class LuaTableExpression : Expression {
    public string OpenBraceToken => Tokens.OpenBrace;
    public readonly LuaSyntaxList<LuaTableItemSyntax> Items = new();
    public string CloseBraceToken => Tokens.CloseBrace;
    public bool IsSingleLine { get; set; }

    public LuaTableExpression() {
    }

    public LuaTableExpression(IEnumerable<Expression> expressions) {
      Items.AddRange(expressions.Select(i => new LuaSingleTableItemSyntax(i)));
    }

    internal void Add(IdentifierNameExpression key, Expression value) {
      Items.Add(new LuaKeyValueTableItemSyntax(key, value));
    }

    internal void Add(Expression value) {
      Items.Add(new LuaSingleTableItemSyntax(value));
    }

    internal void AddRange(IEnumerable<Expression> values) {
      Items.AddRange(values.Select(i => new LuaSingleTableItemSyntax(i)));
    }

    public static readonly LuaTableExpression Empty = new();

    public Expression GetSingleExpression(int index) {
      var item = (LuaSingleTableItemSyntax)Items[index];
      return item.Expression;
    }
  }

  public abstract class LuaTableItemSyntax : LuaSyntaxNode {
  }

  public sealed class LuaSingleTableItemSyntax : LuaTableItemSyntax {
    public Expression Expression { get; }

    public LuaSingleTableItemSyntax(Expression expression) {
      Expression = expression ?? throw new ArgumentNullException(nameof(expression));
    }

  }

  public abstract class LuaTableKeySyntax : LuaSyntaxNode {
  }

  public sealed class LuaTableExpressionKeySyntax : LuaTableKeySyntax {
    public Expression Expression { get; }
    public string OpenBracketToken => Tokens.OpenBracket;
    public string CloseBracketToken => Tokens.CloseBracket;

    public LuaTableExpressionKeySyntax(Expression expression) {
      Expression = expression ?? throw new ArgumentNullException(nameof(expression));
    }
  }

  public sealed class LuaTableLiteralKeySyntax : LuaTableKeySyntax {
    public IdentifierNameExpression Identifier { get; }

    public LuaTableLiteralKeySyntax(IdentifierNameExpression identifier) {
      Identifier = identifier;
    }
  }

  public sealed class LuaKeyValueTableItemSyntax : LuaTableItemSyntax {
    public LuaTableKeySyntax Key { get; }
    public string OperatorToken => Tokens.Equals;
    public Expression Value { get; }

    public LuaKeyValueTableItemSyntax(LuaTableKeySyntax key, Expression value) {
      Key = key;
      Value = value;
    }

    public LuaKeyValueTableItemSyntax(IdentifierNameExpression key, Expression value) : this(new LuaTableLiteralKeySyntax(key), value) {
    }
  }

  public sealed class LuaTableIndexAccessExpressionSyntax : Expression {
    public Expression Expression { get; }
    public Expression Index { get; }
    public string OpenBracketToken => OpenBracket;
    public string CloseBracketToken => CloseBracket;

    public LuaTableIndexAccessExpressionSyntax(Expression expression, Expression index) {
      Expression = expression ?? throw new ArgumentNullException(nameof(expression));
      Index = index ?? throw new ArgumentNullException(nameof(index));
    }
    
  }
}
