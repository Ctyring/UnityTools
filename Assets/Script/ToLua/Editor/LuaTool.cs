using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Script.ToLua.Editor
{
    public class LuaTool
    {
        public static string[] Split(string s, bool isPath = true) {
            HashSet<string> list = new HashSet<string>();
            if (!string.IsNullOrEmpty(s)) {
                string[] array = s.Split(';', ',');
                foreach (string i in array) {
                    list.Add(isPath ? GetCurrentDirectory(i) : i);
                }
            }
            return list.ToArray();
        }
        
        public static string GetCurrentDirectory(string path) {
            const string CurrentDirectorySign1 = "~/";
            const string CurrentDirectorySign2 = "~\\";

            if (path.StartsWith(CurrentDirectorySign1)) {
                return Path.Combine(AppDomain.CurrentDomain.BaseDirectory, path[CurrentDirectorySign1.Length..]);
            }

            if (path.StartsWith(CurrentDirectorySign2)) {
                return Path.Combine(AppDomain.CurrentDomain.BaseDirectory, path[CurrentDirectorySign2.Length..]);
            }

            return Path.Combine(Environment.CurrentDirectory, path);
        }
        
        public static int IndexOf<T>(IEnumerable<T> source, Predicate<T> match) {
            int index = 0;
            foreach (var item in source) {
                if (match(item)) {
                    return index;
                }
                ++index;
            }
            return -1;
        }
        
        public static int IndexOf<T>(IEnumerable<T> source, T value) {
            var comparer = EqualityComparer<T>.Default;
            return IndexOf(source, i => comparer.Equals(i, value));
        }
    }
}