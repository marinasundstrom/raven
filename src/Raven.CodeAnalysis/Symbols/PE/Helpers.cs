using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

public static class ExtensionPropertyReflection
{
    public sealed record ExtensionPropertyGroup(
        string Name,
        MethodInfo? GetMethod,
        MethodInfo? SetMethod);

    public static IReadOnlyList<ExtensionPropertyGroup> GroupByAccessorConvention(TypeInfo typeInfo)
    {
        // key = (optional explicit-interface prefix, property name)
        var map = new Dictionary<(string? prefix, string name), (MethodInfo? get, MethodInfo? set)>();

        foreach (var m in typeInfo.DeclaredMethods)
        {
            if (!m.IsSpecialName) // property accessors are SpecialName in IL
                continue;

            var (prefix, propName, kind) = TryParseAccessorName(m.Name);
            if (propName is null)
                continue;

            // Optional: if these are "extension properties", you'll likely only want static accessors
            // (since extension props are lowered to static methods).
            if (!m.IsStatic)
                continue;

            // Optional: filter out non-property-like signatures.
            // get_X(self) -> T
            // set_X(self, value) -> void / Unit
            if (kind == AccessorKind.Get)
            {
                if (m.GetParameters().Length < 1) continue;
                // you may want: m.ReturnType != typeof(void)
            }
            else if (kind == AccessorKind.Set)
            {
                if (m.GetParameters().Length < 2) continue;
                // you may want: m.ReturnType == typeof(void)
            }

            var key = (prefix, propName);

            map.TryGetValue(key, out var pair);
            if (kind == AccessorKind.Get)
                pair.get ??= m;
            else
                pair.set ??= m;

            map[key] = pair;
        }

        // Materialize results
        var list = new List<ExtensionPropertyGroup>(map.Count);
        foreach (var kv in map)
        {
            var (prefix, name) = kv.Key;
            var (get, set) = kv.Value;

            if (get is null && set is null)
                continue;

            // If you want to preserve explicit interface grouping in metadata, you can include prefix in Name
            // but usually you'd keep Name = property name only and store prefix elsewhere.
            list.Add(new ExtensionPropertyGroup(name, get, set));
        }

        return list;
    }

    private enum AccessorKind { Get, Set, None }

    private static (string? prefix, string? propName, AccessorKind kind) TryParseAccessorName(string methodName)
    {
        // Handles "get_Foo", "set_Foo", and "IFoo.get_Foo" / "IFoo.set_Foo"
        string? prefix = null;
        string simple = methodName;

        var lastDot = methodName.LastIndexOf('.');
        if (lastDot >= 0)
        {
            prefix = methodName[..lastDot];
            simple = methodName[(lastDot + 1)..];
        }

        const string get_ = "get_";
        const string set_ = "set_";

        if (simple.StartsWith(get_, StringComparison.Ordinal))
            return (prefix, simple[get_.Length..], AccessorKind.Get);

        if (simple.StartsWith(set_, StringComparison.Ordinal))
            return (prefix, simple[set_.Length..], AccessorKind.Set);

        return (null, null, AccessorKind.None);
    }
}
