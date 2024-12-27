using Microsoft.CodeAnalysis;

public static class SyntaxNodeExtensions
{
    /// <summary>
    /// Joins a collection of SyntaxNodes with a specified SyntaxToken as a separator.
    /// </summary>
    /// <param name="nodes">The collection of SyntaxNodes to join.</param>
    /// <param name="separator">The SyntaxToken to insert between nodes.</param>
    /// <returns>A SyntaxNode representing the joined nodes.</returns>
    public static IEnumerable<SyntaxNodeOrToken> JoinWithSeparator(this IEnumerable<SyntaxNode> nodes, SyntaxToken separator)
    {
        if (nodes == null || !nodes.Any())
            return null;

        // Flatten the nodes and separators into a single list
        var joinedNodes = new List<SyntaxNodeOrToken>();
        var nodeList = nodes.ToList();

        for (int i = 0; i < nodeList.Count; i++)
        {
            joinedNodes.Add(nodeList[i]);
            if (i < nodeList.Count - 1) // Add separator between nodes
            {
                joinedNodes.Add(separator);
            }
        }

        // Return a single SyntaxNode combining the nodes and separators
        return joinedNodes;
    }
}