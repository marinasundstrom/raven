using System.Reflection;

namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxNodeReflectionExtensions
{
    public static Type? GetPropertyTypeForChild(this SyntaxNode parent, SyntaxNode childNode)
    {
        if (childNode == null)
            throw new ArgumentNullException(nameof(childNode));

        // Find properties of the current node
        var properties = parent.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

        foreach (var property in properties)
        {
            var propertyValue = property.GetValue(parent);

            // Direct match: property equals childNode
            // Comparing green nodes since actual green node might differ.
            if (propertyValue is SyntaxNode node && node == childNode)
            {
                return property.PropertyType;
            }

            // Handle lists: check if childNode is contained within a SyntaxList<T> or SeparatedSyntaxList<T>
            if (propertyValue is IEnumerable<object> collection)
            {
                if (collection.Cast<SyntaxNode>().Contains(childNode))
                {
                    // Extract the generic type parameter from the list type
                    var propertyType = property.PropertyType;
                    if (propertyType.IsGenericType)
                    {
                        return propertyType.GetGenericArguments().FirstOrDefault();
                    }
                }
            }
        }

        return null; // No matching property found
    }

    public static string? GetPropertyNameForChild(this SyntaxNode parent, SyntaxNode childNode)
    {
        if (childNode == null)
            throw new ArgumentNullException(nameof(childNode));

        // Find properties of the current node
        var properties = parent.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

        foreach (var property in properties)
        {
            if (property.Name == "ContainsDiagnostics")
                continue;

            var propertyValue = property.GetValue(parent);

            // Direct match: property equals childNode
            // Comparing green nodes since actual green node might differ.
            if (propertyValue is SyntaxNode node && node == childNode)
            {
                return property.Name;
            }
        }

        return null; // No matching property found
    }

    public static string? GetPropertyNameForChild(this SyntaxNode parent, SyntaxToken childNode)
    {
        if (childNode == null)
            throw new ArgumentNullException(nameof(childNode));

        // Find properties of the current node
        var properties = parent.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

        foreach (var property in properties)
        {
            var propertyValue = property.GetValue(parent);

            // Direct match: property equals childNode
            // Comparing green nodes since actual green node might differ.
            if (propertyValue is SyntaxToken node && node == childNode)
            {
                return property.Name;
            }
        }

        return null; // No matching property found
    }

    public static IEnumerable<IGrouping<string, SyntaxNodeOrToken>> GetChildrenGroupedByPropertyName(this SyntaxNode node, bool includeTokens = true)
    {
        var children = node.ChildNodesAndTokens();
        var groups = new List<(string Name, SyntaxNodeOrToken Child)>();

        foreach (var child in children)
        {
            string? name = null;

            if (child.IsNode && child.AsNode() is SyntaxNode childNode)
            {
                if (child.ParentListGreen is not null)
                {
                    name = childNode.Parent?.GetPropertyNameForListItem(childNode);

                }
                else
                {
                    name = childNode.Parent?.GetPropertyNameForChild(childNode);
                }
            }
            else if (includeTokens && child.IsToken && child.AsToken() is SyntaxToken token)
            {
                if (child.ParentListGreen is not null)
                {
                    name = token.Parent?.GetPropertyNameForListItem(token);

                }
                else
                {
                    name = token.Parent?.GetPropertyNameForChild(token);
                }
            }

            if (!string.IsNullOrEmpty(name))
                groups.Add((name!, child));
        }

        return groups.GroupBy(x => x.Name, x => x.Child);
    }

    public static string? GetPropertyNameForListItem(this SyntaxNode node, SyntaxNode nodeItem)
    {
        var properties = node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

        foreach (var property in properties)
        {
            if (typeof(IEnumerable<SyntaxNode>).IsAssignableFrom(property.PropertyType))
            {
                var value = property.GetValue(node) as IEnumerable<SyntaxNode>;
                if (value != null && value.Contains(nodeItem))
                    return property.Name;
            }
        }

        return null;
    }

    public static string? GetPropertyNameForListItem(this SyntaxNode node, SyntaxToken tokenItem)
    {
        var properties = node.GetType().GetProperties(BindingFlags.Public | BindingFlags.Instance);

        foreach (var property in properties)
        {
            var value = property.GetValue(node);

            if (value is SyntaxList<SyntaxNode> nodeList)
            {
                foreach (var item in nodeList)
                {
                    var tokens = item.ChildNodesAndTokens()
                        .Where(x => x.IsToken)
                        .Select(x => x.AsToken());
                    if (tokens.Contains(tokenItem))
                        return property.Name;
                }
            }
            else if (value is SeparatedSyntaxList<SyntaxNode> sepList)
            {
                foreach (var item in sepList.GetWithSeparators())
                {
                    if (item.IsToken && item.AsToken() == tokenItem)
                        return property.Name;
                    else if (item.IsNode && item.AsNode()?.ChildNodesAndTokens()
                        .Where(x => x.IsToken)
                        .Select(x => x.AsToken())
                        .Contains(tokenItem) == true)
                        return property.Name;
                }
            }
        }

        return null;
    }
}