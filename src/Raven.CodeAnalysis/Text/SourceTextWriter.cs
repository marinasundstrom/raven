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

            var leadingWidth = Math.Min(leadingTrivia.Width, builder.Length);
            if (leadingWidth > 0)
                builder.Remove(0, leadingWidth);

            var trailingLength = Math.Min(trailingTrivia.Width, builder.Length);
            var trailingStart = builder.Length - trailingLength;
            if (trailingLength > 0)
                builder.Remove(trailingStart, trailingLength);
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
