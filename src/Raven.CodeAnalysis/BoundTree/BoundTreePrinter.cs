using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public static class BoundTreePrinter
{
    private static readonly SymbolDisplayFormat BoundTreeDisplayFormat = SymbolDisplayFormat.MinimallyQualifiedFormat
        .WithGenericsOptions(SymbolDisplayGenericsOptions.IncludeTypeParameters)
        .WithLocalOptions(SymbolDisplayLocalOptions.IncludeBinding | SymbolDisplayLocalOptions.IncludeType | SymbolDisplayLocalOptions.IncludeConstantValue)
        .WithParameterOptions(/* SymbolDisplayParameterOptions.IncludeBinding | */ SymbolDisplayParameterOptions.IncludeType | SymbolDisplayParameterOptions.IncludeModifiers | SymbolDisplayParameterOptions.IncludeName)
        .WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypes)
        .WithMemberOptions(SymbolDisplayMemberOptions.IncludeContainingType | SymbolDisplayMemberOptions.IncludeType | SymbolDisplayMemberOptions.IncludeParameters | SymbolDisplayMemberOptions.IncludeModifiers); // | SymbolDisplayMemberOptions.IncludeAccessibility)

    public static bool Colorize { get; set; } = true;

    private static string MaybeColorize(string text, AnsiColor color)
        => Colorize ? ColorizeText(text, color) : text;

    private static string ColorizeText(string text, AnsiColor color)
        => $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";

    public static void PrintBoundTree(
        this SemanticModel model,
        bool includeChildPropertyNames = false,
        bool groupChildCollections = false,
        bool displayCollectionIndices = false,
        bool onlyBlockRoots = true,
        bool includeErrorNodes = false,
        bool includeBinderInfo = true,
        bool includeBinderChainOnRoots = true,
        bool showBinderOnlyOnChange = true,
        bool colorize = true,
        BoundTreeView view = BoundTreeView.Original)
    {
        if (model is null)
            throw new ArgumentNullException(nameof(model));

        Colorize = colorize;

        if (view is BoundTreeView.Both)
        {
            Console.WriteLine(MaybeColorize("=== Original Bound Tree ===", AnsiColor.BrightCyan));
            PrintBoundTree(model, includeChildPropertyNames, groupChildCollections, displayCollectionIndices, onlyBlockRoots, includeErrorNodes, includeBinderInfo, includeBinderChainOnRoots, showBinderOnlyOnChange, colorize, BoundTreeView.Original);
            Console.WriteLine();
            Console.WriteLine(MaybeColorize("=== Lowered Bound Tree ===", AnsiColor.BrightCyan));
            PrintBoundTree(model, includeChildPropertyNames, groupChildCollections, displayCollectionIndices, onlyBlockRoots, includeErrorNodes, includeBinderInfo, includeBinderChainOnRoots, showBinderOnlyOnChange, colorize, BoundTreeView.Lowered);
            return;
        }

        var cache = GetBoundNodeCache(model, view);
        if (cache.Count == 0)
        {
            ForceBind(model, view);
            cache = GetBoundNodeCache(model, view);
        }

        if (cache.Count == 0)
        {
            Console.WriteLine("<no bound nodes>");
            return;
        }

        var nodeToSyntax = BuildNodeToSyntaxMap(cache);
        var roots = GetSyntacticRootNodes(cache);
        PopulateMissingSyntaxMappings(model, roots, nodeToSyntax);

        var visitedNodes = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);
        var visitedSyntaxes = new HashSet<SyntaxNode>(ReferenceEqualityComparer.Instance);

        // Binder id map and id generator
        var binderIds = new Dictionary<Binder, int>(ReferenceEqualityComparer.Instance);
        var nextBinderId = 0;

        int getBinderId(Binder b)
        {
            if (binderIds.TryGetValue(b, out var existing))
                return existing;

            // Assign parents first so IDs increase down the chain.
            if (b.ParentBinder is not null)
                _ = getBinderId(b.ParentBinder);

            var id = nextBinderId++;
            binderIds.Add(b, id);
            return id;
        }

        var printedAny = false;

        for (var i = 0; i < roots.Count; i++)
        {
            var root = roots[i];

            // Skip error-only roots (usually artifacts of speculative binding / lookup noise)
            if (!includeErrorNodes && IsErrorRoot(root))
                continue;

            // We are only concerned with blocks: compilation-unit blocks and method-body blocks.
            if (onlyBlockRoots && !IsBlockRoot(root))
                continue;

            // If this root corresponds to a syntax we've already printed somewhere
            // in another tree, skip it.
            if (nodeToSyntax.TryGetValue(root, out var syntax) &&
                visitedSyntaxes.Contains(syntax.Syntax))
            {
                continue;
            }

            if (printedAny)
                Console.WriteLine();
            printedAny = true;

            PrintRoot(root, nodeToSyntax, visitedNodes, visitedSyntaxes, binderIds, getBinderId, includeBinderInfo, includeBinderChainOnRoots);
            PrintChildren(root, nodeToSyntax, string.Empty, includeChildPropertyNames, groupChildCollections, displayCollectionIndices, visitedNodes, visitedSyntaxes, binderIds, getBinderId, includeBinderInfo, showBinderOnlyOnChange, parentBinderId: null);
        }
    }

    private static bool IsBlockRoot(BoundNode node)
    {
        // Covers BoundBlockStatement and any future BoundBlock* node kinds.
        return node.GetType().Name.StartsWith("BoundBlock", StringComparison.Ordinal);
    }

    private static bool IsErrorRoot(BoundNode node)
    {
        if (node is BoundErrorExpression)
            return true;

        if (node is BoundExpression expr && expr.Type is { } t && t.TypeKind == TypeKind.Error)
            return true;

        return false;
    }

    private readonly record struct ChildEntry(string? Name, BoundNode? Node, IReadOnlyList<ChildEntry>? Children)
    {
        public bool IsGroup => Node is null;

        public static ChildEntry ForNode(BoundNode node, string? name)
            => new(name, node, Children: null);

        public static ChildEntry ForGroup(string name, IReadOnlyList<ChildEntry> children)
            => new(name, Node: null, children);
    }

    /*
    private static Dictionary<SyntaxNode, BoundNode> GetBoundNodeCache(SemanticModel model)
    {
        var field = typeof(SemanticModel).GetField("_boundNodeCache", BindingFlags.NonPublic | BindingFlags.Instance);
        return (Dictionary<SyntaxNode, BoundNode>)field!.GetValue(model)!;
    }*/

    private static Dictionary<SyntaxNode, (Binder, BoundNode)> GetBoundNodeCache(SemanticModel model, BoundTreeView view)
    {
        var fieldName = view switch
        {
            BoundTreeView.Original => "_boundNodeCache2",
            BoundTreeView.Lowered => "_loweredBoundNodeCache2",
            _ => "_boundNodeCache2"
        };
        var field = typeof(SemanticModel).GetField(fieldName, BindingFlags.NonPublic | BindingFlags.Instance);
        return (Dictionary<SyntaxNode, (Binder, BoundNode)>)field!.GetValue(model)!;
    }

    private static void ForceBind(SemanticModel model, BoundTreeView view)
    {
        var root = model.SyntaxTree.GetRoot();
        foreach (var node in root.DescendantNodesAndSelf())
        {
            switch (node)
            {
                case StatementSyntax:
                case ExpressionSyntax:
                    try
                    {
                        model.GetBoundNode(node, view);
                    }
                    catch
                    {
                        // Ignore nodes that cannot be bound (e.g. declarations).
                    }

                    break;
            }
        }
    }

    private static Dictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> BuildNodeToSyntaxMap(
     Dictionary<SyntaxNode, (Binder Binder, BoundNode Node)> cache)
    {
        var map = new Dictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)>(ReferenceEqualityComparer.Instance);

        foreach (var kvp in cache)
        {
            var syntax = kvp.Key;
            var binder = kvp.Value.Binder;
            var node = kvp.Value.Node;

            if (!map.ContainsKey(node))
                map[node] = (syntax, binder);
        }

        return map;
    }

    private static void PopulateMissingSyntaxMappings(
        SemanticModel model,
        IEnumerable<BoundNode> roots,
        Dictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax)
    {
        var visited = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);
        foreach (var root in roots)
            PopulateMissingSyntaxMappings(model, root, nodeToSyntax, visited, parentBinder: null);
    }

    private static void PopulateMissingSyntaxMappings(
        SemanticModel model,
        BoundNode node,
        Dictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax,
        HashSet<BoundNode> visited,
        Binder? parentBinder)
    {
        if (!visited.Add(node))
            return;

        Binder? binderForChildren = parentBinder;

        if (!nodeToSyntax.TryGetValue(node, out var info))
        {
            var syntax = model.GetSyntax(node);
            if (syntax is not null)
            {
                var binder = parentBinder ?? model.GetBinder(syntax);
                info = (syntax, binder);
                nodeToSyntax[node] = info;
            }
        }

        if (nodeToSyntax.TryGetValue(node, out info))
            binderForChildren = info.Binder;

        foreach (var child in EnumerateChildNodes(GetChildren(node, includeChildPropertyNames: false, groupChildCollections: false, displayCollectionIndices: false)))
            PopulateMissingSyntaxMappings(model, child, nodeToSyntax, visited, binderForChildren);
    }

    private static List<BoundNode> GetSyntacticRootNodes(Dictionary<SyntaxNode, (Binder Binder, BoundNode Node)> cache)
    {
        // A syntactic root is a bound node whose syntax span is not strictly contained
        // within another cached syntax span that was bound by the *same binder instance*.
        // This keeps separate roots for different binder regions (e.g. top-level vs method body).

        var entries = cache.Select(kvp => (Syntax: kvp.Key, Binder: kvp.Value.Binder, Node: kvp.Value.Node)).ToList();

        static bool ContainsStrict(TextSpan outer, TextSpan inner)
            => outer.Start <= inner.Start && outer.End >= inner.End && (outer.Start != inner.Start || outer.End != inner.End);

        var roots = new List<(SyntaxNode Syntax, Binder Binder, BoundNode Node)>();

        foreach (var e in entries)
        {
            var isContainedBySameBinder = false;

            foreach (var other in entries)
            {
                if (ReferenceEquals(other.Syntax, e.Syntax))
                    continue;

                if (!ReferenceEquals(other.Binder, e.Binder))
                    continue;

                if (ContainsStrict(other.Syntax.Span, e.Syntax.Span))
                {
                    isContainedBySameBinder = true;
                    break;
                }
            }

            if (!isContainedBySameBinder)
                roots.Add(e);
        }

        roots.Sort((a, b) =>
        {
            var cmp = a.Syntax.Span.Start.CompareTo(b.Syntax.Span.Start);
            if (cmp != 0)
                return cmp;

            // If same start, prefer the larger span first.
            cmp = b.Syntax.Span.Length.CompareTo(a.Syntax.Span.Length);
            if (cmp != 0)
                return cmp;

            return string.Compare(a.Node.GetType().Name, b.Node.GetType().Name, StringComparison.Ordinal);
        });

        // Deduplicate in case multiple syntax nodes map to the same bound node.
        var unique = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);
        var result = new List<BoundNode>();
        foreach (var r in roots)
        {
            if (unique.Add(r.Node))
                result.Add(r.Node);
        }

        return result;
    }

    private static int CompareSyntax(
     IReadOnlyDictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax,
     BoundNode left,
     BoundNode right)
    {
        var hasLeft = nodeToSyntax.TryGetValue(left, out var leftInfo);
        var hasRight = nodeToSyntax.TryGetValue(right, out var rightInfo);

        if (!hasLeft && !hasRight)
            return string.Compare(left.GetType().Name, right.GetType().Name, StringComparison.Ordinal);
        if (!hasLeft)
            return -1;
        if (!hasRight)
            return 1;

        var comparison = leftInfo.Syntax.Span.Start.CompareTo(rightInfo.Syntax.Span.Start);
        if (comparison != 0)
            return comparison;

        return string.Compare(left.GetType().Name, right.GetType().Name, StringComparison.Ordinal);
    }

    private static void PrintRoot(
        BoundNode node,
        IReadOnlyDictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax,
        HashSet<BoundNode> visitedNodes,
        HashSet<SyntaxNode> visitedSyntaxes,
        Dictionary<Binder, int> binderIds,
        Func<Binder, int> getBinderId,
        bool includeBinderInfo,
        bool includeBinderChainOnRoots)
    {
        var alreadyVisitedNode = !visitedNodes.Add(node);

        var description = Describe(node, nodeToSyntax, includeBinderInfo, getBinderId);
        if (alreadyVisitedNode)
            description += " [cycle]";

        // Root prints without tree marker.
        Console.WriteLine(description);

        if (includeBinderInfo && includeBinderChainOnRoots && nodeToSyntax.TryGetValue(node, out var rootInfo))
        {
            var chain = FormatBinderChain(rootInfo.Binder, getBinderId);
            if (!string.IsNullOrWhiteSpace(chain))
                Console.WriteLine($"  {MaybeColorize("BinderChain", AnsiColor.BrightGreen)}: {MaybeColorize(chain, AnsiColor.BrightBlack)}");
        }

        if (alreadyVisitedNode)
            return;

        if (nodeToSyntax.TryGetValue(node, out var syntax))
            visitedSyntaxes.Add(syntax.Syntax);
    }

    private static IEnumerable<BoundNode> EnumerateChildNodes(IEnumerable<ChildEntry> entries)
    {
        foreach (var entry in entries)
        {
            if (!entry.IsGroup)
            {
                yield return entry.Node!;
                continue;
            }

            if (entry.Children is null)
                continue;

            foreach (var child in EnumerateChildNodes(entry.Children))
                yield return child;
        }
    }

    private static void PrintChildren(
        BoundNode node,
        IReadOnlyDictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax,
        string indent,
        bool includeChildPropertyNames,
        bool groupChildCollections,
        bool displayCollectionIndices,
        HashSet<BoundNode> visitedNodes,
        HashSet<SyntaxNode> visitedSyntaxes,
        Dictionary<Binder, int> binderIds,
        Func<Binder, int> getBinderId,
        bool includeBinderInfo,
        bool showBinderOnlyOnChange,
        int? parentBinderId)
    {
        var children = GetChildren(node, includeChildPropertyNames, groupChildCollections, displayCollectionIndices).ToList();
        for (var i = 0; i < children.Count; i++)
        {
            PrintRecursive(
                children[i],
                nodeToSyntax,
                indent,
                i == children.Count - 1,
                includeChildPropertyNames,
                groupChildCollections,
                displayCollectionIndices,
                visitedNodes,
                visitedSyntaxes,
                binderIds,
                getBinderId,
                includeBinderInfo,
                showBinderOnlyOnChange,
                parentBinderId);
        }
    }

    private static void PrintRecursive(
        ChildEntry child,
        IReadOnlyDictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax,
        string indent,
        bool isLast,
        bool includeChildPropertyNames,
        bool groupChildCollections,
        bool displayCollectionIndices,
        HashSet<BoundNode> visitedNodes,
        HashSet<SyntaxNode> visitedSyntaxes,
        Dictionary<Binder, int> binderIds,
        Func<Binder, int> getBinderId,
        bool includeBinderInfo,
        bool showBinderOnlyOnChange,
        int? parentBinderId)
    {
        var markerRaw = isLast ? "└── " : "├── ";
        var marker = MaybeColorize(markerRaw, AnsiColor.BrightBlack);

        // Group entry (for collection properties)
        if (child.IsGroup)
        {
            var groupName = child.Name ?? "<group>";
            var groupText = includeChildPropertyNames ? MaybeColorize(groupName, AnsiColor.BrightBlue) : groupName;
            Console.WriteLine($"{indent}{marker}{groupText}");

            if (child.Children is null || child.Children.Count == 0)
                return;

            for (var i = 0; i < child.Children.Count; i++)
            {
                var childIndent = indent + (isLast ? "    " : "│   ");
                PrintRecursive(
                    child.Children[i],
                    nodeToSyntax,
                    childIndent,
                    i == child.Children.Count - 1,
                    includeChildPropertyNames,
                    groupChildCollections,
                    displayCollectionIndices,
                    visitedNodes,
                    visitedSyntaxes,
                    binderIds,
                    getBinderId,
                    includeBinderInfo,
                    showBinderOnlyOnChange,
                    parentBinderId);
            }

            return;
        }

        var node = child.Node!;
        var alreadyVisitedNode = !visitedNodes.Add(node);

        int? currentBinderId = null;
        if (includeBinderInfo && nodeToSyntax.TryGetValue(node, out var infoForBinder))
            currentBinderId = getBinderId(infoForBinder.Binder);

        var printBinder = includeBinderInfo && (!showBinderOnlyOnChange || parentBinderId is null || currentBinderId != parentBinderId);
        var description = Describe(node, nodeToSyntax, includeBinderInfo: printBinder, getBinderId);
        var hasName = !string.IsNullOrEmpty(child.Name);
        var isIndexName = hasName && child.Name!.StartsWith("[", StringComparison.Ordinal);
        if ((includeChildPropertyNames && hasName) || (displayCollectionIndices && isIndexName))
        {
            var nameColor = includeChildPropertyNames ? AnsiColor.BrightGreen : AnsiColor.BrightBlack;
            description = $"{MaybeColorize(child.Name!, nameColor)}: {description}";
        }
        if (alreadyVisitedNode)
            description += " [cycle]";

        Console.WriteLine($"{indent}{marker}{description}");

        if (alreadyVisitedNode)
            return;

        // Mark this node's syntax as visited (if any)
        if (nodeToSyntax.TryGetValue(node, out var syntax))
            visitedSyntaxes.Add(syntax.Syntax);

        var children = GetChildren(node, includeChildPropertyNames, groupChildCollections, displayCollectionIndices).ToList();
        for (var i = 0; i < children.Count; i++)
        {
            var childIndent = indent + (isLast ? "    " : "│   ");
            PrintRecursive(
                children[i],
                nodeToSyntax,
                childIndent,
                i == children.Count - 1,
                includeChildPropertyNames,
                groupChildCollections,
                displayCollectionIndices,
                visitedNodes,
                visitedSyntaxes,
                binderIds,
                getBinderId,
                includeBinderInfo,
                showBinderOnlyOnChange,
                currentBinderId);
        }
    }

    private static IEnumerable<ChildEntry> GetChildren(BoundNode node, bool includeChildPropertyNames, bool groupChildCollections, bool displayCollectionIndices)
    {
        var visitedContainers = new HashSet<object>(ReferenceEqualityComparer.Instance);

        foreach (var property in node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance))
        {
            if (property.GetIndexParameters().Length > 0)
                continue;

            if (ShouldSkipProperty(property.Name))
                continue;

            var value = property.GetValue(node);
            if (value is null)
                continue;

            // When requested, group enumerable children under a single property header.
            if (groupChildCollections && value is IEnumerable enumerable && value is not string)
            {
                if (IsDefaultImmutableArray(value))
                    continue;

                var grouped = new List<ChildEntry>();
                var index = 0;
                foreach (var item in enumerable)
                {
                    var itemName = displayCollectionIndices ? $"[{index}]" : null;
                    foreach (var child in EnumerateBoundNodeChildren(item, visitedContainers, itemName, displayCollectionIndices))
                        grouped.Add(child);
                    index++;
                }

                if (grouped.Count > 0)
                    yield return ChildEntry.ForGroup(property.Name, grouped);

                continue;
            }

            if (!groupChildCollections && displayCollectionIndices && value is IEnumerable enumerable2 && value is not string)
            {
                if (IsDefaultImmutableArray(value))
                    continue;

                var index = 0;
                foreach (var item in enumerable2)
                {
                    var itemName = includeChildPropertyNames ? $"{property.Name}[{index}]" : $"[{index}]";
                    foreach (var child in EnumerateBoundNodeChildren(item, visitedContainers, itemName, displayCollectionIndices))
                        yield return child;
                    index++;
                }

                continue;
            }

            foreach (var child in EnumerateBoundNodeChildren(value, visitedContainers, includeChildPropertyNames ? property.Name : null, displayCollectionIndices))
                yield return child;
        }
    }

    private static IEnumerable<ChildEntry> EnumerateBoundNodeChildren(object? value, HashSet<object> visitedContainers, string? name, bool displayCollectionIndices)
    {
        if (value is null)
            yield break;

        if (value is BoundNode bound)
        {
            yield return ChildEntry.ForNode(bound, name);
            yield break;
        }

        if (value is IEnumerable enumerable && value is not string)
        {
            if (IsDefaultImmutableArray(value))
                yield break;

            // For nested enumerables, keep the same edge name and flow through.
            var index = 0;
            foreach (var item in enumerable)
            {
                var itemName = name;
                if (displayCollectionIndices)
                {
                    itemName = name is null ? $"[{index}]" : $"{name}[{index}]";
                }

                foreach (var child in EnumerateBoundNodeChildren(item, visitedContainers, itemName, displayCollectionIndices))
                    yield return child;

                index++;
            }

            yield break;
        }

        var type = value.GetType();

        if (type.IsPrimitive || value is string || value is Enum)
            yield break;

        if (typeof(ISymbol).IsAssignableFrom(type) || typeof(ITypeSymbol).IsAssignableFrom(type))
            yield break;

        if (!string.Equals(type.Namespace, typeof(BoundNode).Namespace, StringComparison.Ordinal))
            yield break;

        if (!type.Name.StartsWith("Bound", StringComparison.Ordinal))
            yield break;

        if (!visitedContainers.Add(value))
            yield break;

        foreach (var property in type.GetProperties(BindingFlags.Public | BindingFlags.Instance))
        {
            if (property.GetIndexParameters().Length > 0)
                continue;

            if (ShouldSkipProperty(property.Name))
                continue;

            // Preserve the original edge name when walking through wrapper objects.
            foreach (var child in EnumerateBoundNodeChildren(property.GetValue(value), visitedContainers, name, displayCollectionIndices))
                yield return child;
        }
    }

    private static bool IsDefaultImmutableArray(object value)
    {
        var type = value.GetType();
        if (!type.IsGenericType || type.GetGenericTypeDefinition() != typeof(ImmutableArray<>))
            return false;

        return (bool)(type.GetProperty("IsDefault")?.GetValue(value) ?? false);
    }

    private static string Describe(
        BoundNode node,
        IReadOnlyDictionary<BoundNode, (SyntaxNode Syntax, Binder Binder)> nodeToSyntax,
        bool includeBinderInfo,
        Func<Binder, int> getBinderId)
    {
        var name = node.GetType().Name;
        if (name.StartsWith("Bound", StringComparison.Ordinal))
            name = name["Bound".Length..];

        var coloredName = MaybeColorize(name, AnsiColor.Yellow);

        var details = new List<string>();

        static string K(string key, string value, Func<string, string> k, Func<string, string> v)
            => $"{k(key)}={v(value)}";

        if (nodeToSyntax.TryGetValue(node, out var info))
        {
            var kindStr = info.Syntax.Kind.ToString();
            kindStr = $"{kindStr} [{RenderSourceSpan(info)}]";
            details.Add(K("Syntax", kindStr, s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightBlue)));

            if (includeBinderInfo)
            {
                var id = getBinderId(info.Binder);
                details.Add(K("Binder", $"#{id} {FormatBinder(info.Binder)}", s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightBlack)));
            }
        }

        switch (node)
        {
            case BoundExpression expression:
                if (expression.Type is { } type)
                    details.Add(K("Type", FormatType(type), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Magenta)));
                if (expression.Symbol is { } exprSymbol)
                    details.Add(K("Symbol", FormatSymbol(exprSymbol), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Cyan)));
                if (expression.Reason != BoundExpressionReason.None)
                    details.Add(K("Reason", expression.Reason.ToString(), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightBlue)));
                break;
            case BoundStatement statement:
                if (statement.Symbol is { } stmtSymbol)
                    details.Add(K("Symbol", FormatSymbol(stmtSymbol), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Cyan)));
                break;
        }

        foreach (var property in node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance))
        {
            if (property.GetIndexParameters().Length > 0)
                continue;

            if (ShouldSkipProperty(property.Name))
                continue;

            var propertyType = property.PropertyType;

            if (typeof(BoundNode).IsAssignableFrom(propertyType))
                continue;

            if (typeof(IEnumerable).IsAssignableFrom(propertyType) && propertyType != typeof(string))
            {
                var enumerableValue = property.GetValue(node);
                if (enumerableValue is null)
                    continue;

                if (propertyType.IsGenericType && propertyType.GetGenericTypeDefinition() == typeof(ImmutableArray<>))
                {
                    var isDefault = (bool)(enumerableValue.GetType().GetProperty("IsDefault")?.GetValue(enumerableValue) ?? false);
                    if (isDefault)
                        continue;
                }

                var items = new List<string>();
                var containsBoundNodes = false;

                foreach (var item in (IEnumerable)enumerableValue)
                {
                    if (item is null)
                        continue;

                    if (item is BoundNode)
                    {
                        containsBoundNodes = true;
                        break;
                    }

                    if (item is ISymbol symbolItem)
                        items.Add(MaybeColorize(FormatSymbol(symbolItem), AnsiColor.Cyan));
                    else if (item is ITypeSymbol typeItem)
                        items.Add(MaybeColorize(FormatType(typeItem), AnsiColor.Magenta));
                    else if (item is string stringItem)
                        items.Add(MaybeColorize("\"" + stringItem + "\"", AnsiColor.BrightRed));
                    else if (item is bool boolItem)
                        items.Add(MaybeColorize(boolItem ? "true" : "false", AnsiColor.BrightBlue));
                    else if (item is Enum enumItem)
                        items.Add(MaybeColorize(enumItem.ToString(), AnsiColor.BrightBlue));
                    else if (item.GetType().IsPrimitive)
                        items.Add(MaybeColorize(item.ToString()!, AnsiColor.Red));
                    else
                    {
                        containsBoundNodes = true;
                        break;
                    }
                }

                if (!containsBoundNodes && items.Count > 0)
                    details.Add($"{MaybeColorize(property.Name, AnsiColor.BrightGreen)}=[{string.Join(", ", items)}]");

                continue;
            }

            var value = property.GetValue(node);
            if (value is null)
                continue;

            if (value is ISymbol symbolValue)
            {
                details.Add(K(property.Name, FormatSymbol(symbolValue), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Cyan)));
                continue;
            }

            if (value is ITypeSymbol typeValue)
            {
                details.Add(K(property.Name, FormatType(typeValue), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Magenta)));
                continue;
            }

            if (value is string stringValue)
            {
                details.Add(K(property.Name, "\"" + stringValue + "\"", s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightRed)));
                continue;
            }

            if (value is bool boolValue)
            {
                details.Add(K(property.Name, boolValue ? "true" : "false", s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightBlue)));
                continue;
            }

            if (value is Enum enumValue)
            {
                details.Add(K(property.Name, enumValue.ToString(), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightBlue)));
                continue;
            }

            if (propertyType.IsPrimitive)
            {
                details.Add(K(property.Name, value.ToString()!, s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Red)));
                continue;
            }

            if (propertyType == typeof(object))
            {
                details.Add(K(property.Name, value.ToString()!, s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.Black)));
            }
        }

        return details.Count > 0 ? $"{coloredName} [{string.Join(", ", details)}]" : coloredName;
    }

    private static string RenderSourceSpan((SyntaxNode Syntax, Binder Binder) info)
    {
        // Format like: (1, 2) - (1, 3)
        // Note: Line/character positions are 0-based in Roslyn-style APIs; print as 1-based.
        var location = info.Syntax?.GetLocation();

        if (location is null || (!location.IsInSource && !location.IsInMetadata))
            return "(<Detached node>)";

        var span = location.GetLineSpan();

        var start = span.StartLinePosition;
        var end = span.EndLinePosition;

        return $"({start.Line + 1}, {start.Character + 1}) - ({end.Line + 1}, {end.Character + 1})";
    }

    private static string FormatBinder(Binder binder)
    {
        // Mirror BinderTreePrinter formatting, but don't apply ANSI here.
        // The caller already applies coloring to the whole value.

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

        return binder switch
        {
            GlobalBinder => kind,

            NamespaceBinder ns =>
                kind + " (" + (ns.NamespaceSymbol.IsGlobalNamespace
                    ? "<global>"
                    : ns.NamespaceSymbol?.ToDisplayString() ?? "?") + ")",

            ImportBinder => kind,

            TopLevelBinder => kind + " (synthesized Main)",

            TypeDeclarationBinder td =>
                kind + " (" + (td.ContainingSymbol?.Name ?? "?") + ")",

            MethodBinder m =>
                kind + " (" + (m.GetMethodSymbol()?.Name ?? "?") + ")",

            // For binders where we don't have easy symbol/context access here,
            // fall back to ToString() when it provides extra information.
            _ => FormatBinderFallback(kind, binder)
        };
    }

    private static string FormatBinderFallback(string kind, Binder binder)
    {
        var text = binder.ToString();

        if (string.IsNullOrWhiteSpace(text))
            return kind;

        if (string.Equals(text, binder.GetType().Name, StringComparison.Ordinal) ||
            string.Equals(text, binder.GetType().FullName, StringComparison.Ordinal) ||
            string.Equals(text, kind, StringComparison.Ordinal))
            return kind;

        return kind + " (" + text + ")";
    }

    private static string FormatBinderChain(Binder binder, Func<Binder, int> getBinderId)
    {
        var parts = new List<string>();
        for (var current = binder; current is not null; current = current.ParentBinder)
        {
            var id = getBinderId(current);
            parts.Add($"#{id} {FormatBinder(current)}");
        }

        // We collected leaf->root; reverse to show root->leaf.
        parts.Reverse();
        return string.Join(" → ", parts);
    }

    private static bool ShouldSkipProperty(string propertyName)
        => propertyName is nameof(BoundExpression.Type)
            or nameof(BoundExpression.Symbol)
            or nameof(BoundExpression.Reason)
            or nameof(BoundStatement.Symbol);

    private static string FormatType(ITypeSymbol type)
    {
        return type.ToDisplayString(BoundTreeDisplayFormat);
    }

    private static string FormatSymbol(ISymbol symbol)
    {
        return symbol.ToDisplayString(BoundTreeDisplayFormat);
    }
}
