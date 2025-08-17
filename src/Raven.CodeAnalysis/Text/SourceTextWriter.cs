using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public static class SourceTextWriter
{
    public static string WriteNodeToText(SyntaxNode node, bool withSpans = false)
    {
        var builder = new StringBuilder();

        WriteNodeToText(node, builder);
        if (!withSpans)
        {
            var leadingTrivia = node.GetLeadingTrivia();
            var trailingTrivia = node.GetTrailingTrivia();

            builder.Remove(0, leadingTrivia.Width);
            builder.Remove(node.Width, trailingTrivia.Width);
        }
        return builder.ToString();
    }

    private static void WriteNodeToText(SyntaxNode node, StringBuilder builder)
    {
        if (node is null)
            return;

        // Recursively write child nodes
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.TryGetToken(out var token))
            {
                builder.Append(token.ToFullString());
            }
            else if (child.TryGetNode(out var childNode))
            {
                WriteNodeToText(childNode, builder);
            }
        }
    }
}