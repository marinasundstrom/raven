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

    public static NodeProperties GetChildrenGroupedByProperty(this SyntaxNode node, bool includeTokens = true)
    {
        var children = node.ChildNodesAndTokens();
        var groups = new Dictionary<string, List<Child>>();

        var listProperties = new HashSet<string>();

        int slotIndex = 0;

        foreach (var child in children)
        {
            string? name = null;
            bool isListItem = child.ParentListGreen is not null;

            if (child.IsNode && child.AsNode() is SyntaxNode childNode)
            {
                name = isListItem
                    ? childNode.Parent?.GetPropertyNameForListItem(childNode)
                    : childNode.Parent?.GetPropertyNameForChild(childNode);
            }
            else if (includeTokens && child.IsToken && child.AsToken() is SyntaxToken token)
            {
                name = isListItem
                    ? token.Parent?.GetPropertyNameForListItem(token)
                    : token.Parent?.GetPropertyNameForChild(token);
            }

            if (!string.IsNullOrEmpty(name))
            {
                if (!groups.TryGetValue(name!, out var list))
                    groups[name!] = list = new List<Child>();

                list.Add(new Child
                {
                    SlotIndex = slotIndex++,
                    Item = child
                });

                if (isListItem)
                    listProperties.Add(name!);
            }
        }

        var properties = groups.Select(kvp =>
        {
            return new ChildGroup
            {
                PropertyName = kvp.Key,
                IsList = listProperties.Contains(kvp.Key),
                Items = kvp.Value
            };
        });

        return new NodeProperties(properties);
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
            else if (value is SyntaxTokenList tokenList)
            {
                foreach (var item in tokenList)
                {
                    return property.Name;
                }
            }
        }

        return null;
    }

    public sealed class NodeProperties
    {
        private readonly IEnumerable<ChildGroup> _lazy;
        private IReadOnlyList<ChildGroup> _properties;
        private int? _slotCount;
        private IDictionary<int, Child> _map;

        public NodeProperties(IEnumerable<ChildGroup> properties)
        {
            _lazy = properties;
        }

        public int SlotCount => _slotCount ??= Properties.SelectMany(x => x.Items).Count();

        public IReadOnlyList<ChildGroup> Properties => _properties ??= _lazy.ToList();

        public Child this[int index]
        {
            get
            {
                if (_map is null)
                {
                    _map = CreateMap();
                }

                return _map[index];
            }
        }

        private IDictionary<int, Child> CreateMap()
        {
            Dictionary<int, Child> dictionary = new Dictionary<int, Child>();

            foreach (var group in Properties)
            {
                foreach (var child in group.Items)
                {
                    dictionary[child.SlotIndex] = child;
                }
            }

            return dictionary;
        }
    }

    public sealed class ChildGroup
    {
        public required string PropertyName { get; init; }
        public required bool IsList { get; init; }
        public required IReadOnlyList<Child> Items { get; init; }
    }

    public sealed class Child
    {
        public int SlotIndex { get; init; }

        public required SyntaxNodeOrToken Item { get; init; }

        public override string ToString() => $"{SlotIndex}: {Item.AsNode()?.Kind ?? Item.AsToken().Kind}";
    }
}