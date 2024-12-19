using System.Text;

namespace Raven.CodeAnalysis.Syntax;

public static class SourceTextWriter
{
    public static string WriteNodeToText(SyntaxNode node, bool withSpans = false)
    {
        var builder = new StringBuilder();

        WriteNodeToText(node, builder, withSpans);
        return builder.ToString();
    }

    private static void WriteNodeToText(SyntaxNode node, StringBuilder builder, bool withSpans)
    {
        if (node == null)
            return;

        // Recursively write child nodes
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.AsToken(out var token))
            {
                builder.Append(token.ToFullString());
            }
            else if (child.AsNode(out var childNode))
            {
                WriteNodeToText(childNode, builder, withSpans);
            }
        }
    }
}