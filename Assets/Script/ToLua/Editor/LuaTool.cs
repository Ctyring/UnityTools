using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis;

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
        
        /// <summary>
        /// 判断definition是否包含type类型
        /// </summary>
        /// <param name="definition"></param>
        /// <param name="type"></param>
        /// <returns></returns>
        public static bool IsContainsType(INamedTypeSymbol definition, ISymbol type) {
            if (SymbolEqualityComparer.Default.Equals(type.ContainingType, definition)) {
                return true;
            }

            var containingType = type.ContainingType;
            if (containingType == null) {
                return false;
            }

            return IsContainsType(definition, containingType);
        }
        
        /// <summary>
        /// 判断type是否依赖于other
        /// </summary>
        /// <param name="type"></param>
        /// <param name="other"></param>
        /// <returns></returns>
        public static bool IsDependExists(INamedTypeSymbol type, INamedTypeSymbol other) {
            if (SymbolEqualityComparer.Default.Equals(type, other)) {
                return true;
            }

            foreach (var typeArgument in type.TypeArguments) {
                if (typeArgument.Kind != SymbolKind.TypeParameter) {
                    if (SymbolEqualityComparer.Default.Equals(typeArgument.OriginalDefinition, other)) {
                        return true;
                    }
                }
            }

            if (type.BaseType != null && !IsSystemObjectOrValueType(type.BaseType)) {
                if (IsDependExists(type.BaseType, other)) {
                    return true;
                }
            }

            foreach (var interfaceType in type.Interfaces) {
                if (IsDependExists(interfaceType, other)) {
                    return true;
                }
            }

            return false;
        }
        
        public static bool IsSystemObjectOrValueType(INamedTypeSymbol symbol) {
            return symbol.SpecialType is SpecialType.System_Object or SpecialType.System_ValueType;
        }
        
        public sealed class ConcurrentHashSet<T> : IEnumerable<T> {
            private readonly ConcurrentDictionary<T, bool> dict_;

            public ConcurrentHashSet() {
                dict_ = new();
            }

            public ConcurrentHashSet(IEqualityComparer<T> comparer) {
                dict_ = new(comparer);
            }

            public bool Add(T v) {
                return dict_.TryAdd(v, true);
            }

            public bool Contains(T v) {
                return dict_.ContainsKey(v);
            }

            public IEnumerator<T> GetEnumerator() {
                return dict_.Keys.GetEnumerator();
            }

            IEnumerator IEnumerable.GetEnumerator() {
                return GetEnumerator();
            }

            public void UnionWith(IEnumerable<T> collection) {
                foreach (var v in collection) {
                    Add(v);
                }
            }
        }
    }
}