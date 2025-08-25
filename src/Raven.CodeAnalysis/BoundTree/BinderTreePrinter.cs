using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Binder = Raven.CodeAnalysis.Binder;

public static class BinderTreePrinter
{
    public static void PrintBinderTree(this SemanticModel model)
    {
        var cache = GetBinderCache(model);
        var allBinders = cache.Values.Distinct().ToHashSet();

        // Discover all reachable parents (even if not in the cache)
        foreach (var binder in cache.Values)
        {
            var current = binder.ParentBinder;
            while (current is not null && allBinders.Add(current))
                current = current.ParentBinder;
        }

        var parentToChildren = BuildParentChildMap(allBinders);

        // Get true root(s)
        var roots = allBinders
            .Where(b => b.ParentBinder is null)
            .OrderBy(DescribeBinder)
            .ToList();

        foreach (var root in roots)
            PrintRecursive(root, parentToChildren, "", isLast: true);
    }

    private static Dictionary<Binder, List<Binder>> BuildParentChildMap(IEnumerable<Binder> allBinders)
    {
        var map = new Dictionary<Binder, List<Binder>>();

        foreach (var binder in allBinders)
        {
            if (binder.ParentBinder is { } parent)
            {
                if (!map.TryGetValue(parent, out var list))
                    map[parent] = list = new List<Binder>();

                list.Add(binder);
            }
        }

        return map;
    }

    private static void PrintRecursive(Binder binder, Dictionary<Binder, List<Binder>> parentToChildren, string indent, bool isLast)
    {
        var marker = isLast ? "└── " : "├── ";
        Console.WriteLine($"{indent}{marker}{DescribeBinder(binder)}");

        indent += isLast ? "    " : "│   ";

        if (parentToChildren.TryGetValue(binder, out var children))
        {
            for (int i = 0; i < children.Count; i++)
                PrintRecursive(children[i], parentToChildren, indent, i == children.Count - 1);
        }
    }

    private static string DescribeBinder(Binder binder)
    {
        return binder switch
        {
            GlobalBinder => "GlobalBinder",
            NamespaceBinder ns => $"NamespaceBinder ({(ns.NamespaceSymbol.IsGlobalNamespace ? "<global>" : ns.NamespaceSymbol?.ToDisplayString())})",
            ImportBinder => "ImportBinder",
            TopLevelBinder => "TopLevelBinder (synthesized Main)",
            TypeDeclarationBinder td => $"TypeDeclarationBinder ({td.ContainingSymbol?.Name ?? "?"})",
            MethodBinder m => $"MethodBinder ({m.GetMethodSymbol()?.Name ?? "?"})",
            MethodBodyBinder => "MethodBodyBinder",
            BlockBinder => "BlockBinder",
            LocalScopeBinder => "LocalScopeBinder",
            LocalFunctionBinder => "LocalFunctionBinder",
            _ => binder.GetType().Name
        };
    }

    private static Dictionary<SyntaxNode, Binder> GetBinderCache(SemanticModel model)
    {
        var field = typeof(SemanticModel).GetField("_binderCache", BindingFlags.NonPublic | BindingFlags.Instance);
        return (Dictionary<SyntaxNode, Binder>)field!.GetValue(model)!;
    }
}
