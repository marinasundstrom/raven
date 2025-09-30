using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public static class BoundTreePrinter
{
    public static void PrintBoundTree(this SemanticModel model)
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
        var visited = new HashSet<BoundNode>(ReferenceEqualityComparer.Instance);

        for (var i = 0; i < roots.Count; i++)
        {
            if (i > 0)
                Console.WriteLine();

            PrintRecursive(roots[i], nodeToSyntax, string.Empty, i == roots.Count - 1, visited);
        }
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
            foreach (var child in GetChildren(node))
                children.Add(child);
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

    private static void PrintRecursive(
        BoundNode node,
        IReadOnlyDictionary<BoundNode, SyntaxNode> nodeToSyntax,
        string indent,
        bool isLast,
        HashSet<BoundNode> visited)
    {
        var marker = isLast ? "└── " : "├── ";
        var alreadyVisited = !visited.Add(node);
        var description = Describe(node, nodeToSyntax);

        if (alreadyVisited)
            description += " [cycle]";

        Console.WriteLine($"{indent}{marker}{description}");

        if (alreadyVisited)
            return;

        var children = GetChildren(node).ToList();
        for (var i = 0; i < children.Count; i++)
        {
            var childIndent = indent + (isLast ? "    " : "│   ");
            PrintRecursive(children[i], nodeToSyntax, childIndent, i == children.Count - 1, visited);
        }
    }

    private static IEnumerable<BoundNode> GetChildren(BoundNode node)
    {
        var visitedContainers = new HashSet<object>(ReferenceEqualityComparer.Instance);

        foreach (var property in node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance))
        {
            if (property.GetIndexParameters().Length > 0)
                continue;

            if (ShouldSkipProperty(property.Name))
                continue;

            foreach (var child in EnumerateBoundNodeChildren(property.GetValue(node), visitedContainers))
                yield return child;
        }
    }

    private static IEnumerable<BoundNode> EnumerateBoundNodeChildren(object? value, HashSet<object> visitedContainers)
    {
        if (value is null)
            yield break;

        if (value is BoundNode bound)
        {
            yield return bound;
            yield break;
        }

        if (value is IEnumerable enumerable && value is not string)
        {
            if (IsDefaultImmutableArray(value))
                yield break;

            foreach (var item in enumerable)
            {
                foreach (var child in EnumerateBoundNodeChildren(item, visitedContainers))
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

            foreach (var child in EnumerateBoundNodeChildren(property.GetValue(value), visitedContainers))
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

        var details = new List<string>();
        if (nodeToSyntax.TryGetValue(node, out var syntax))
            details.Add($"Syntax={syntax.Kind}");

        switch (node)
        {
            case BoundExpression expression:
                if (expression.Type is { } type)
                    details.Add($"Type={FormatType(type)}");
                if (expression.Symbol is { } exprSymbol)
                    details.Add($"Symbol={FormatSymbol(exprSymbol)}");
                if (expression.Reason != BoundExpressionReason.None)
                    details.Add($"Reason={expression.Reason}");
                break;
            case BoundStatement statement:
                if (statement.Symbol is { } stmtSymbol)
                    details.Add($"Symbol={FormatSymbol(stmtSymbol)}");
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
                        items.Add(FormatSymbol(symbolItem));
                    else if (item is ITypeSymbol typeItem)
                        items.Add(FormatType(typeItem));
                    else if (item is string stringItem)
                        items.Add($"\"{stringItem}\"");
                    else if (item is bool boolItem)
                        items.Add(boolItem ? "true" : "false");
                    else if (item is Enum enumItem)
                        items.Add(enumItem.ToString());
                    else if (item.GetType().IsPrimitive)
                        items.Add(item.ToString()!);
                    else
                    {
                        containsBoundNodes = true;
                        break;
                    }
                }

                if (!containsBoundNodes && items.Count > 0)
                    details.Add($"{property.Name}=[{string.Join(", ", items)}]");

                continue;
            }

            var value = property.GetValue(node);
            if (value is null)
                continue;

            if (value is ISymbol symbolValue)
            {
                details.Add($"{property.Name}={FormatSymbol(symbolValue)}");
                continue;
            }

            if (value is ITypeSymbol typeValue)
            {
                details.Add($"{property.Name}={FormatType(typeValue)}");
                continue;
            }

            if (value is string stringValue)
            {
                details.Add($"{property.Name}=\"{stringValue}\"");
                continue;
            }

            if (value is bool boolValue)
            {
                details.Add($"{property.Name}={(boolValue ? "true" : "false")}");
                continue;
            }

            if (value is Enum enumValue)
            {
                details.Add($"{property.Name}={enumValue}");
                continue;
            }

            if (propertyType.IsPrimitive)
            {
                details.Add($"{property.Name}={value}");
                continue;
            }

            if (propertyType == typeof(object))
            {
                details.Add($"{property.Name}={value}");
            }
        }

        return details.Count > 0 ? $"{name} [{string.Join(", ", details)}]" : name;
    }

    private static bool ShouldSkipProperty(string propertyName)
        => propertyName is nameof(BoundExpression.Type)
            or nameof(BoundExpression.Symbol)
            or nameof(BoundExpression.Reason)
            or nameof(BoundStatement.Symbol);

    private static string FormatType(ITypeSymbol type)
    {
        return type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }

    private static string FormatSymbol(ISymbol symbol)
    {
        return symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
    }
}
