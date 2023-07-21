namespace Script.ToLua.Editor.luaAst {
    public class LongCommentStatement: CommentStatement {
        public string Text { get; }
        public int EqualsCount { get; }
        public string OpenBracket => LuaDefine.Tokens.OpenBracket;
        public string CloseBracket => LuaDefine.Tokens.CloseBracket;

        public LongCommentStatement(string value, bool checkNewLine = true) {
            char equals = LuaDefine.Tokens.Equals[0];
            int count = 0;
            // 用=处理多行注释中的[]冲突
            while (true) {
                string equalsToken = new string(equals, count);
                if (value.StartsWith(equalsToken + OpenBracket)) {
                    ++count;
                    continue;
                }

                if (value.EndsWith(equalsToken + CloseBracket)) {
                    ++count;
                    continue;
                }

                if (value.Contains(OpenBracket + equalsToken + OpenBracket)) {
                    ++count;
                    continue;
                }

                if (value.Contains(CloseBracket + equalsToken + CloseBracket)) {
                    ++count;
                    continue;
                }

                break;
            }
            if (checkNewLine) {
                if (value.Length > 0 && value[0] == '\n') {
                    value = '\n' + value;
                }
            }
            Text = value;
            EqualsCount = count;
        }
    }
}