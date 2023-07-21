using System;
using System.Diagnostics;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Script.ToLua.Editor.luaAst;

namespace Script.ToLua.Editor {
    public partial class LuaSyntaxTreeBuilder {
        public ValExpression GetConstExpression(ExpressionSyntax node) {
            var constValue = _semanticModel.GetConstantValue(node);
            if (constValue.HasValue) {
                switch (constValue.Value) {
                    case double d:
                        switch (d) {
                            case double.NegativeInfinity:
                            case double.PositiveInfinity:
                            case double.NaN:
                                return null;
                        }
                        break;
                    case float f:
                        switch (f) {
                            case float.NegativeInfinity:
                            case float.PositiveInfinity:
                            case float.NaN:
                                return null;
                        }
                        break;
                    case null:
                        return new ConstExpression(LuaDefine.IdentifierName.Nil.name, "nil");
                }
                
                ValExpression expression = GetLiteralExpression(constValue.Value);
                return new ConstExpression(expression.value, node.ToString());
            }
            return null;
        }
        
        private ValExpression GetLiteralExpression(object constantValue) {
            if (constantValue != null) {
                var code = Type.GetTypeCode(constantValue.GetType());
                switch (code) {
                    case TypeCode.Char: {
                        Debug.Assert(constantValue != null, nameof(constantValue) + " != null");
                        return new CharExpression(((int)constantValue).ToString(), SyntaxFactory.Literal((char)constantValue).Text);
                    }
                    case TypeCode.String: {
                        return BuildStringLiteralExpression((string)constantValue);
                    }
                    case TypeCode.Boolean: {
                        bool v = (bool)constantValue;
                        return v ? new ValExpression(LuaDefine.IdentifierName.True.name, LuaDefine.IdentifierName.True) : new ValExpression(LuaDefine.IdentifierName.False.name, LuaDefine.IdentifierName.False);
                    }
                    case TypeCode.Single: {
                        float v = (float)constantValue;
                        return new FloatExpression(v);
                    }
                    case TypeCode.Double: {
                        double v = (double)constantValue;
                        return new DoubleExpression(v);
                    }
                    case TypeCode.Int64: {
                        if (constantValue is long.MinValue) {
                            const long kMinInteger = long.MinValue + 1;     // in lua5.4 long.MinValue will be float
                            return new IdentifierExpression($"({kMinInteger} - 1)");
                        }
                        break;
                    }
                }
                return new IdentifierExpression(constantValue.ToString());
            }

            return new ValExpression(LuaDefine.IdentifierName.Nil.name, LuaDefine.IdentifierName.Nil);
        }
        
        private StringExpression BuildStringLiteralExpression(string value) {
            string text = SyntaxFactory.Literal(value).Text;
            return new StringExpression(text);
        }
    }
}