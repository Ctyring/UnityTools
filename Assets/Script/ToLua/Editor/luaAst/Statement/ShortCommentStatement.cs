namespace Script.ToLua.Editor.luaAst
{
    public class ShortCommentStatement: CommentStatement
    {
        public string token => LuaDefine.Tokens.ShortComment;
        public string content;
        public ShortCommentStatement(string content)
        {
            this.content = content;
        }
    }
}