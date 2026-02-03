using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Binder = Raven.CodeAnalysis.Binder;

public static class BinderTreePrinter
{
    public static bool Colorize { get; set; } = true;

    private static string MaybeColorize(string text, AnsiColor color)
        => Colorize ? ColorizeText(text, color) : text;

    private static string ColorizeText(string text, AnsiColor color)
        => $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";

    public static void PrintBinderTree(this SemanticModel model)
    {
        var cache = GetBinderCache(model);
        var binderNodes = cache
            .GroupBy(kv => kv.Value)
            .ToDictionary(g => g.Key, g => g.Select(kv => kv.Key).ToList());

        var allBinders = binderNodes.Keys.Distinct().ToHashSet();

        // Discover all reachable parents (even if not in the cache)
        foreach (var binder in binderNodes.Keys)
        {
            var current = binder.ParentBinder;
            while (current is not null && allBinders.Add(current))
                current = current.ParentBinder;
        }

        var parentToChildren = BuildParentChildMap(allBinders);

        // Get true root(s)
        var roots = allBinders
            .Where(b => b.ParentBinder is null)
            .OrderBy(b => DescribeBinder(b, binderNodes, parentToChildren))
            .ToList();

        foreach (var root in roots)
        {
            PrintRoot(root, parentToChildren, binderNodes);
            PrintChildren(root, parentToChildren, binderNodes, "");
        }
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

    private static void PrintRoot(
        Binder binder,
        Dictionary<Binder, List<Binder>> parentToChildren,
        Dictionary<Binder, List<SyntaxNode>> binderNodes)
    {
        // Root prints without tree marker.
        Console.WriteLine(DescribeBinder(binder, binderNodes, parentToChildren));
    }

    private static void PrintChildren(
        Binder binder,
        Dictionary<Binder, List<Binder>> parentToChildren,
        Dictionary<Binder, List<SyntaxNode>> binderNodes,
        string indent)
    {
        if (parentToChildren.TryGetValue(binder, out var children))
        {
            for (int i = 0; i < children.Count; i++)
                PrintRecursive(children[i], parentToChildren, binderNodes, indent, i == children.Count - 1);
        }
    }

    private static void PrintRecursive(
        Binder binder,
        Dictionary<Binder, List<Binder>> parentToChildren,
        Dictionary<Binder, List<SyntaxNode>> binderNodes,
        string indent,
        bool isLast)
    {
        var markerRaw = isLast ? "└── " : "├── ";
        var marker = MaybeColorize(markerRaw, AnsiColor.BrightBlack);
        Console.WriteLine($"{indent}{marker}{DescribeBinder(binder, binderNodes, parentToChildren)}");

        indent += isLast ? "    " : "│   ";

        if (parentToChildren.TryGetValue(binder, out var children))
        {
            for (int i = 0; i < children.Count; i++)
                PrintRecursive(children[i], parentToChildren, binderNodes, indent, i == children.Count - 1);
        }
    }

    private static string DescribeBinder(
        Binder binder,
        Dictionary<Binder, List<SyntaxNode>> binderNodes,
        Dictionary<Binder, List<Binder>> parentToChildren)
    {
        static string DescribeSymbol(
            Binder binder,
            Dictionary<Binder, List<SyntaxNode>> nodesMap,
            Dictionary<Binder, List<Binder>> parentToChildren)
        {
            if (nodesMap.TryGetValue(binder, out var nodes) && nodes.Count > 0)
            {
                foreach (var node in nodes)
                {
                    var symbol = binder.BindDeclaredSymbol(node);
                    if (symbol is not null)
                        return $" ({MaybeColorize(symbol.ToDisplayString(), AnsiColor.Cyan)})";
                }
            }
            else if (parentToChildren.TryGetValue(binder, out var children))
            {
                foreach (var child in children)
                {
                    if (nodesMap.TryGetValue(child, out var childNodes))
                    {
                        foreach (var node in childNodes)
                        {
                            var symbol = binder.BindDeclaredSymbol(node);
                            if (symbol is not null)
                                return $" ({MaybeColorize(symbol.ToDisplayString(), AnsiColor.Cyan)})";
                        }
                    }
                }
            }

            return string.Empty;
        }

        var kind = binder switch
        {
            GlobalBinder => "GlobalBinder",
            NamespaceBinder => "NamespaceBinder",
            ImportBinder => "ImportBinder",
            TopLevelBinder => "TopLevelBinder",
            TypeDeclarationBinder => "TypeDeclarationBinder",
            MethodBinder => "MethodBinder",
            TypeMemberBinder => "TypeMemberBinder",
            MethodBodyBinder => "MethodBodyBinder",
            BlockBinder => "BlockBinder",
            LocalScopeBinder => "LocalScopeBinder",
            FunctionBinder => "FunctionBinder",
            _ => binder.GetType().Name
        };

        // Kind in yellow so the tree reads like a legend.
        var coloredKind = MaybeColorize(kind, AnsiColor.Yellow);

        return binder switch
        {
            GlobalBinder => coloredKind,

            NamespaceBinder ns =>
                coloredKind + " (" + MaybeColorize(ns.NamespaceSymbol.IsGlobalNamespace ? "<global>" : ns.NamespaceSymbol?.ToDisplayString() ?? "?", AnsiColor.Magenta) + ")",

            ImportBinder => coloredKind,

            TopLevelBinder => coloredKind + " (" + MaybeColorize("synthesized Main", AnsiColor.BrightBlue) + ")",

            TypeDeclarationBinder td =>
                coloredKind + " (" + MaybeColorize(td.ContainingSymbol?.ToDisplayString() ?? "?", AnsiColor.Cyan) + ")",

            MethodBinder m =>
                coloredKind + " (" + MaybeColorize(m.GetMethodSymbol()?.ToDisplayString() ?? "?", AnsiColor.Cyan) + ")",

            TypeMemberBinder => coloredKind + DescribeSymbol(binder, binderNodes, parentToChildren),

            MethodBodyBinder => coloredKind,
            BlockBinder => coloredKind,
            LocalScopeBinder => coloredKind,

            FunctionBinder => coloredKind + DescribeSymbol(binder, binderNodes, parentToChildren),

            _ => MaybeColorize(binder.GetType().Name, AnsiColor.Yellow)
        };
    }

    private static Dictionary<SyntaxNode, Binder> GetBinderCache(SemanticModel model)
    {
        var field = typeof(SemanticModel).GetField("_binderCache", BindingFlags.NonPublic | BindingFlags.Instance);
        return (Dictionary<SyntaxNode, Binder>)field!.GetValue(model)!;
    }
}
