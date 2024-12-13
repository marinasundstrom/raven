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
}