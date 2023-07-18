using UnityEditor;
using UnityEngine;
namespace Script.ToLua.Editor
{
    public class TestCecil : EditorWindow
    {
        public static string HotFixNameSpace = "HotFix";
        
        [MenuItem("测试/生成ast")]
        public static void Test()
        {
            LuaGenerator generator = new LuaGenerator();
            generator.BuildCompilation();
        }
    }
}