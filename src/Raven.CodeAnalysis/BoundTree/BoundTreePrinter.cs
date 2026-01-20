using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using System;

using Raven.CodeAnalysis.Text;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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

    public static void PrintBoundTree(this SemanticModel model, bool includeChildPropertyNames = false)
    {
        if (model is null)
            throw new ArgumentNullException(nameof(model));

        var cache = GetBoundNodeCache(model);
        if (cache.Count == 0)
        {
            ForceBind(model);
            cache = GetBoundNodeCache(model);
        }

        if (cache.Count == 0)
        {
            Console.WriteLine("<no bound nodes>");
            return;
        }

        var nodeToSyntax = BuildNodeToSyntaxMap(cache);
        var roots = GetRootNodes(nodeToSyntax.Keys, nodeToSyntax);

        var visitedNodes = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);
        var visitedSyntaxes = new HashSet<SyntaxNode>(ReferenceEqualityComparer.Instance);

        var printedAny = false;

        for (var i = 0; i < roots.Count; i++)
        {
            var root = roots[i];

            // If this root corresponds to a syntax we've already printed somewhere
            // in another tree, skip it.
            if (nodeToSyntax.TryGetValue(root, out var syntax) &&
                visitedSyntaxes.Contains(syntax))
            {
                continue;
            }

            if (printedAny)
                Console.WriteLine();
            printedAny = true;

            PrintRoot(root, nodeToSyntax, visitedNodes, visitedSyntaxes);
            PrintChildren(root, nodeToSyntax, string.Empty, includeChildPropertyNames, visitedNodes, visitedSyntaxes);
        }
    }

    private readonly record struct ChildEntry(string? Name, BoundNode? Node, IReadOnlyList<ChildEntry>? Children)
    {
        public bool IsGroup => Node is null;

        public static ChildEntry ForNode(BoundNode node, string? name)
            => new(name, node, Children: null);

        public static ChildEntry ForGroup(string name, IReadOnlyList<ChildEntry> children)
            => new(name, Node: null, children);
    }

    private static Dictionary<SyntaxNode, BoundNode> GetBoundNodeCache(SemanticModel model)
    {
        var field = typeof(SemanticModel).GetField("_boundNodeCache", BindingFlags.NonPublic | BindingFlags.Instance);
        return (Dictionary<SyntaxNode, BoundNode>)field!.GetValue(model)!;
    }

    private static void ForceBind(SemanticModel model)
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
                        model.GetBoundNode(node);
                    }
                    catch
                    {
                        // Ignore nodes that cannot be bound (e.g. declarations).
                    }

                    break;
            }
        }
    }

    private static Dictionary<BoundNode, SyntaxNode> BuildNodeToSyntaxMap(Dictionary<SyntaxNode, BoundNode> cache)
    {
        var map = new Dictionary<BoundNode, SyntaxNode>(ReferenceEqualityComparer.Instance);
        foreach (var (syntax, node) in cache)
        {
            if (!map.ContainsKey(node))
                map[node] = syntax;
        }

        return map;
    }

    private static List<BoundNode> GetRootNodes(IEnumerable<BoundNode> nodes, IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax)
    {
        var unique = new HashSet<BoundNode>(nodes, ReferenceEqualityComparer.Instance);
        var children = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);

        foreach (var node in unique)
        {
            foreach (var childNode in EnumerateChildNodes(GetChildren(node, includeChildPropertyNames: false)))
                children.Add(childNode);
        }

        var roots = unique.Where(node => !children.Contains(node)).ToList();
        roots.Sort((left, right) => CompareSyntax(nodeToSyntax, left, right));
        return roots;
    }

    private static int CompareSyntax(IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax, BoundNode left, BoundNode right)
    {
        var hasLeft = nodeToSyntax.TryGetValue(left, out var leftSyntax);
        var hasRight = nodeToSyntax.TryGetValue(right, out var rightSyntax);

        if (!hasLeft && !hasRight)
            return string.Compare(left.GetType().Name, right.GetType().Name, StringComparison.Ordinal);
        if (!hasLeft)
            return -1;
        if (!hasRight)
            return 1;

        var comparison = leftSyntax.Span.Start.CompareTo(rightSyntax.Span.Start);
        if (comparison != 0)
            return comparison;

        return string.Compare(left.GetType().Name, right.GetType().Name, StringComparison.Ordinal);
    }

    private static void PrintRoot(
        BoundNode node,
        IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax,
        HashSet<BoundNode> visitedNodes,
        HashSet<SyntaxNode> visitedSyntaxes)
    {
        var alreadyVisitedNode = !visitedNodes.Add(node);

        var description = Describe(node, nodeToSyntax);
        if (alreadyVisitedNode)
            description += " [cycle]";

        // Root prints without tree marker.
        Console.WriteLine(description);

        if (alreadyVisitedNode)
            return;

        if (nodeToSyntax.TryGetValue(node, out var syntax))
            visitedSyntaxes.Add(syntax);
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
        IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax,
        string indent,
        bool includeChildPropertyNames,
        HashSet<BoundNode> visitedNodes,
        HashSet<SyntaxNode> visitedSyntaxes)
    {
        var children = GetChildren(node, includeChildPropertyNames).ToList();
        for (var i = 0; i < children.Count; i++)
        {
            PrintRecursive(children[i], nodeToSyntax, indent, i == children.Count - 1, includeChildPropertyNames, visitedNodes, visitedSyntaxes);
        }
    }

    private static void PrintRecursive(
        ChildEntry child,
        IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax,
        string indent,
        bool isLast,
        bool includeChildPropertyNames,
        HashSet<BoundNode> visitedNodes,
        HashSet<SyntaxNode> visitedSyntaxes)
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
                PrintRecursive(child.Children[i], nodeToSyntax, childIndent, i == child.Children.Count - 1, includeChildPropertyNames, visitedNodes, visitedSyntaxes);
            }

            return;
        }

        var node = child.Node!;
        var alreadyVisitedNode = !visitedNodes.Add(node);

        var description = Describe(node, nodeToSyntax);
        if (includeChildPropertyNames && !string.IsNullOrEmpty(child.Name))
            description = $"{MaybeColorize(child.Name, AnsiColor.BrightGreen)}: {description}";
        if (alreadyVisitedNode)
            description += " [cycle]";

        Console.WriteLine($"{indent}{marker}{description}");

        if (alreadyVisitedNode)
            return;

        // Mark this node's syntax as visited (if any)
        if (nodeToSyntax.TryGetValue(node, out var syntax))
            visitedSyntaxes.Add(syntax);

        var children = GetChildren(node, includeChildPropertyNames).ToList();
        for (var i = 0; i < children.Count; i++)
        {
            var childIndent = indent + (isLast ? "    " : "│   ");
            PrintRecursive(children[i], nodeToSyntax, childIndent, i == children.Count - 1, includeChildPropertyNames, visitedNodes, visitedSyntaxes);
        }
    }

    private static IEnumerable<ChildEntry> GetChildren(BoundNode node, bool includeChildPropertyNames)
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
            if (includeChildPropertyNames && value is IEnumerable enumerable && value is not string)
            {
                if (IsDefaultImmutableArray(value))
                    continue;

                var grouped = new List<ChildEntry>();
                var index = 0;
                foreach (var item in enumerable)
                {
                    var itemName = string.Empty; //$"[{index}]";
                    foreach (var child in EnumerateBoundNodeChildren(item, visitedContainers, itemName))
                        grouped.Add(child);
                    index++;
                }

                if (grouped.Count > 0)
                    yield return ChildEntry.ForGroup(property.Name, grouped);

                continue;
            }

            foreach (var child in EnumerateBoundNodeChildren(value, visitedContainers, includeChildPropertyNames ? property.Name : null))
                yield return child;
        }
    }

    private static IEnumerable<ChildEntry> EnumerateBoundNodeChildren(object? value, HashSet<object> visitedContainers, string? name)
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
            foreach (var item in enumerable)
            {
                foreach (var child in EnumerateBoundNodeChildren(item, visitedContainers, name))
                    yield return child;
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
            foreach (var child in EnumerateBoundNodeChildren(property.GetValue(value), visitedContainers, name))
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

    private static string Describe(BoundNode node, IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax)
    {
        var name = node.GetType().Name;
        if (name.StartsWith("Bound", StringComparison.Ordinal))
            name = name["Bound".Length..];

        var coloredName = MaybeColorize(name, AnsiColor.Yellow);

        var details = new List<string>();

        static string K(string key, string value, Func<string, string> k, Func<string, string> v)
            => $"{k(key)}={v(value)}";

        if (nodeToSyntax.TryGetValue(node, out var syntax))
            details.Add(K("Syntax", syntax.Kind.ToString(), s => MaybeColorize(s, AnsiColor.BrightGreen), s => MaybeColorize(s, AnsiColor.BrightBlue)));

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
