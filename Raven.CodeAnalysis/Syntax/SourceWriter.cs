namespace Raven.CodeAnalysis.Syntax;

public static class SourceWriter
{
    public static string WriteNodeToText(SyntaxNode node, bool withSpans = false)
    {
        var builder = new System.Text.StringBuilder();
        WriteNodeToText(node, builder, withSpans);
        return builder.ToString();
    }

    private static void WriteNodeToText(SyntaxNode node, System.Text.StringBuilder builder, bool withSpans = false)
    {
        if (node == null)
            return;

        // Write the current node's text

        // Recursively write child nodes
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.AsToken(out var token))
            {
                builder.Append(token.ToFullString());
            }
            else if (child.AsNode(out var childNode))
            {
                WriteNodeToText(childNode, builder, true);
            }
        }
    }
}