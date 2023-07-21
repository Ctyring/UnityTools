namespace Script.ToLua.Editor.luaAst
{
    public static class LuaDefine
    {
        public class IdentifierName
        {
            public static readonly IdentifierNameExpression Empty = "";
            public static readonly IdentifierNameExpression Placeholder = "_";
            public static readonly IdentifierNameExpression One = 1.ToString();
            public static readonly IdentifierNameExpression System = "System";
            public static readonly IdentifierNameExpression Namespace = "namespace";
            public static readonly IdentifierNameExpression Class = "class";
            public static readonly IdentifierNameExpression Struct = "struct";
            public static readonly IdentifierNameExpression Interface = "interface";
            public static readonly IdentifierNameExpression Enum = "enum";
            public static readonly IdentifierNameExpression Value = "value";
            public static readonly IdentifierNameExpression This = "this";
            public static readonly IdentifierNameExpression True = "true";
            public static readonly IdentifierNameExpression False = "false";
            public static readonly IdentifierNameExpression Throw = "System.throw";
            public static readonly IdentifierNameExpression Each = "System.each";
            public static readonly IdentifierNameExpression Object = "System.Object";
            public static readonly IdentifierNameExpression Array = "System.Array";
            public static readonly IdentifierNameExpression MultiArray = "System.MultiArray";
            public static readonly IdentifierNameExpression EmptyArray = "System.Array.Empty";
            public static readonly IdentifierNameExpression Apply = "System.apply";
            public static readonly IdentifierNameExpression StaticCtor = "static";
            public static readonly IdentifierNameExpression Init = "internal";
            public static readonly IdentifierNameExpression Ctor = "__ctor__";
            public static readonly IdentifierNameExpression Inherits = "base";
            public static readonly IdentifierNameExpression Default = "default";
            public static readonly IdentifierNameExpression SystemDefault = "System.default";
            public static readonly IdentifierNameExpression Property = "System.property";
            public static readonly IdentifierNameExpression Event = "System.event";
            public static readonly IdentifierNameExpression SystemVoid = "System.Void";
            public static readonly IdentifierNameExpression Nil = "nil";
            public static readonly IdentifierNameExpression TypeOf = "System.typeof";
            public static readonly IdentifierNameExpression Continue = "continue";
            public static readonly IdentifierNameExpression StringChar = "string.char";
            public static readonly IdentifierNameExpression ToStr = "ToString";
            public static readonly IdentifierNameExpression SystemToString = "System.toString";
            public static readonly IdentifierNameExpression EnumToString = "EnumToString";
            public static readonly IdentifierNameExpression DelegateMake = "System.fn";
            public static readonly IdentifierNameExpression DelegateBind = "System.bind";
            public static readonly IdentifierNameExpression DelegateCombine = "System.DelegateCombine";
            public static readonly IdentifierNameExpression DelegateRemove = "System.DelegateRemove";
            public static readonly IdentifierNameExpression IntegerDiv = "System.div";
            public static readonly IdentifierNameExpression Mod = "System.mod";
            public static readonly IdentifierNameExpression ModFloat = "System.modf";
            public static readonly IdentifierNameExpression BitNot = "System.bnot";
            public static readonly IdentifierNameExpression BitAnd = "System.band";
            public static readonly IdentifierNameExpression BitOr = "System.bor";
            public static readonly IdentifierNameExpression BitXor = "System.xor";
            public static readonly IdentifierNameExpression ShiftRight = "System.sr";
            public static readonly IdentifierNameExpression ShiftLeft = "System.sl";
            public static readonly IdentifierNameExpression Try = "System.try";
            public static readonly IdentifierNameExpression CatchFilter = "System.when";
            public static readonly IdentifierNameExpression Is = "System.is";
            public static readonly IdentifierNameExpression As = "System.as";
            public static readonly IdentifierNameExpression Cast = "System.cast";
            public static readonly IdentifierNameExpression Using = "System.using";
            public static readonly IdentifierNameExpression UsingX = "System.usingX";
            public static readonly IdentifierNameExpression Linq = "Linq";
            public static readonly IdentifierNameExpression SystemLinqEnumerable = "System.Linq.Enumerable";
            public static readonly IdentifierNameExpression Delegate = "System.Delegate";
            public static readonly IdentifierNameExpression Import = "System.import";
            public static readonly IdentifierNameExpression Global = "out";
            public static readonly IdentifierNameExpression Metadata = "__metadata__";
            public static readonly IdentifierNameExpression Fields = "fields";
            public static readonly IdentifierNameExpression Properties = "properties";
            public static readonly IdentifierNameExpression Events = "events";
            public static readonly IdentifierNameExpression Methods = "methods";
            public static readonly IdentifierNameExpression Clone = "__clone__";
            public static readonly IdentifierNameExpression NullableClone = "System.Nullable.clone";
            public static readonly IdentifierNameExpression CopyThis = "__copy__";
            public static readonly IdentifierNameExpression RecordMembers = "__members__";
            public static readonly IdentifierNameExpression ValueType = "System.ValueType";
            public static readonly IdentifierNameExpression DateTime = "System.DateTime";
            public static readonly IdentifierNameExpression TimeSpan = "System.TimeSpan";
            public static readonly IdentifierNameExpression AnonymousType = "System.AnonymousType";
            public static readonly IdentifierNameExpression New = "new";
            public static readonly IdentifierNameExpression SystemNew = "System.new";
            public static readonly IdentifierNameExpression StackAlloc = "System.stackalloc";
            public static readonly IdentifierNameExpression GenericT = "__genericT__";
            public static readonly IdentifierNameExpression Base = "base";
            public static readonly IdentifierNameExpression SystemBase = "System.base";
            public static readonly IdentifierNameExpression Tuple = "System.Tuple";
            public static readonly IdentifierNameExpression ValueTuple = "System.ValueTuple";
            public static readonly IdentifierNameExpression RecordType = "System.RecordType";
            public static readonly IdentifierNameExpression RecordValueType = "System.RecordValueType";
            public static readonly IdentifierNameExpression Deconstruct = "Deconstruct";
            public static readonly IdentifierNameExpression KeyValuePair = "System.KeyValuePair";
            public static readonly IdentifierNameExpression NullableType = "System.Nullable";
            public static readonly IdentifierNameExpression Range = "System.Range";
            public static readonly IdentifierNameExpression Index = "System.Index";
            public static readonly IdentifierNameExpression IndexGetOffset = "System.Index.GetOffset";
            public static readonly IdentifierNameExpression __GC = "__gc";
            public static readonly IdentifierNameExpression __ToString = "__tostring";
            public static readonly IdentifierNameExpression Await = "await";
            public static readonly IdentifierNameExpression AwaitAnything = "Await";
            public static readonly IdentifierNameExpression Async = "async";
            public static readonly IdentifierNameExpression AsyncEach = "System.asynceach";
            public static readonly IdentifierNameExpression MoreManyLocalVarTempTable = "const";
            public static readonly IdentifierNameExpression InterfaceDefaultMethodVar = "extern";
            public static readonly IdentifierNameExpression SystemInit = "System.init";
            public static readonly IdentifierNameExpression InlineReturnLabel = "out";
        }

        public static class Keyword
        {
            public const string And = "and";
            public const string Break = "break";
            public const string Do = "do";
            public const string Else = "else";
            public const string ElseIf = "elseif";
            public const string End = "end";

            public const string False = "false";
            public const string For = "for";
            public const string Function = "function";
            public const string Goto = "goto";
            public const string If = "if";
            public const string In = "in";

            public const string Local = "local";
            public const string Nil = "nil";
            public const string Not = "not";
            public const string Or = "or";
            public const string Repeat = "repeat";
            public const string Return = "return";

            public const string Then = "then";
            public const string True = "true";
            public const string Until = "until";
            public const string While = "while";
        }

        public static class Tokens
        {
            public static string Empty => string.Empty;
            public static string Semicolon = ";";
            public const string OpenParentheses = "(";
            public const string CloseParentheses = ")";
            public const string OpenBrace = "{";
            public const string CloseBrace = "}";
            public const string OpenBracket = "[";
            public const string CloseBracket = "]";
            public const string ObjectColon = ":";
            public const string Dot = ".";
            public const string Quote = "\"";
            public const string SingleQuote = "'";
            public new const string Equals = "=";
            public const string EqualsEquals = "==";
            public const string NotEquals = "~=";
            public const string Yield = "yield";
            public const string Plus = "+";
            public const string Sub = "-";
            public const string Multiply = "*";
            public const string IntegerDiv = "//";
            public const string Div = "/";
            public const string Mod = "%";
            public const string BitAnd = "&";
            public const string BitOr = "|";
            public const string BitXor = "~";
            public const string BitNot = "~";
            public const string ShiftLeft = "<<";
            public const string ShiftRight = ">>";
            public const string ShortComment = "--";
            public const string OpenLongComment = "--[[";
            public const string CloseLongComment = "--]]";
            public const string OpenDoubleBrace = "[[";
            public const string CloseDoubleBrace = "]]";
            public const string OpenSummary = "<summary>";
            public const string CloseSummary = "</summary>";
            public const string Ctor = "ctor";
            public const string This = "this";
            public const string Get = "get";
            public const string Set = "set";
            public const string Add = "add";
            public const string Remove = "remove";
            public const string Index = "index";
            public const string Label = "::";
            public const string Concatenation = "..";
            public const string Params = "...";
        }
    }
}