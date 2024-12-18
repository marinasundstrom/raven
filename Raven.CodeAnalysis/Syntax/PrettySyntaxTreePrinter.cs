using System.Text;

namespace Raven.CodeAnalysis.Syntax;

public static class PrettySyntaxTreePrinter
{
    public static void PrintSyntaxTree(this SyntaxNode node, bool includeTokens = true, bool includeTrivia = false, bool includeSpans = false, bool includeLocation = false, int maxDepth = int.MaxValue)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, true, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth);
        Console.WriteLine(sb.ToString());
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, bool includeTokens = true, bool includeTrivia = false, bool includeSpans = false, bool includeLocation = false, int maxDepth = int.MaxValue)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, false, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth);
        return sb.ToString();
    }

    private static void PrintSyntaxTreeCore(SyntaxNode node, StringBuilder sb, string indent, bool isFirst, bool isLast, bool includeTokens, bool includeTrivia, bool includeSpans, bool includeLocation, int maxDepth, int currentDepth = 0)
    {
        if (currentDepth > maxDepth)
            return;

        // Visual markers for tree structure
        var marker = isLast ? "└── " : isFirst ? string.Empty : "├── ";
        sb.AppendLine($"{indent}{marker}{node.Kind}{(includeSpans ? $" {Span(node.Span)}" : string.Empty)}{(includeLocation ? $" {Location(node.GetLocation())}" : string.Empty)}");

        var newIndent = isFirst ? String.Empty : indent + (isLast ? "    " : "│   ");

        var children = node.ChildNodesAndTokens().ToArray();

        if (!includeTokens)
        {
            children = children.Where(x => x.IsNode).ToArray();
        }

        for (int i = 0; i < children.Length; i++)
        {
            var isChildLast = i == children.Length - 1;
            if (children[i].AsNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, sb, newIndent, false, isChildLast, includeTokens, includeTrivia, includeSpans, includeLocation, maxDepth, currentDepth + 1);
            }
            else if (includeTokens && children[i].AsToken(out var token))
            {
                // Include trivia if specified
                if (includeTrivia)
                {
                    // Leading trivia
                    foreach (var trivia in token.LeadingTrivia)
                    {
                        sb.AppendLine($"{newIndent}{(isChildLast ? "    " : "│   ")}[Leading Trivia] {trivia.Kind}: \"{TriviaToString(trivia)}\"");
                    }
                }

                // Print token
                sb.AppendLine($"{newIndent}{(isChildLast ? "└── " : "├── ")}{token.Kind}{(token.IsMissing ? " (Missing)" : "")} \"{token.Text}\"{(includeSpans ? $" {Span(token.Span)}" : string.Empty)}{(includeLocation ? $" {Location(token.GetLocation())}" : string.Empty)}");

                // Include trivia if specified
                if (includeTrivia)
                {
                    // Trailing trivia
                    foreach (var trivia in token.TrailingTrivia)
                    {
                        sb.AppendLine($"{newIndent}{(isChildLast ? "    " : "│   ")}[Trailing Trivia] {trivia.Kind}: \"{TriviaToString(trivia)}\"");
                    }
                }
            }
        }
    }

    private static string Location(Location location)
    {
        return $"({location.Line}:{location.Column})";
    }

    private static string Span(TextSpan span)
    {
        return $"[{span}]";
    }

    private static string TriviaToString(SyntaxTrivia trivia)
    {
        return trivia.ToString().Replace("\r", @"\r")
            .Replace("\n", @"\n")
            .Replace("\t", @"\t");
    }
}
