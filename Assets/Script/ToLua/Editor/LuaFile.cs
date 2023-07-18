namespace Script.ToLua.Editor
{
    public class LuaFile
    {
        public string className = "";
        // 父类列表
        public string[] baseClassList = null;
        
        // 方法列表
        public LuaFuction[] fuctionList = null;
        
        public bool isMain = false;
        public bool isStatic = false;
    }
    
    public class LuaFuction
    {
        public string name = "";
        public string returnType = "";
        public string[] paramList = null;
    }
}