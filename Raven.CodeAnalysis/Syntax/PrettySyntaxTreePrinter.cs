using System.Text;

namespace Raven.CodeAnalysis.Syntax;

public static class PrettySyntaxTreePrinter
{
    public static void PrintSyntaxTree(this SyntaxNode node, bool includeTokens = false, bool includeTrivia = false, int maxDepth = int.MaxValue)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", true, includeTokens, includeTrivia, maxDepth);
        Console.WriteLine(sb.ToString());
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, bool includeTokens = false, bool includeTrivia = false, int maxDepth = int.MaxValue)
    {
        var sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, "", false, includeTokens, includeTrivia, maxDepth);
        return sb.ToString();
    }

    private static void PrintSyntaxTreeCore(SyntaxNode node, StringBuilder sb, string indent, bool isLast, bool includeTokens, bool includeTrivia, int maxDepth, int currentDepth = 0)
    {
        if (currentDepth > maxDepth)
            return;

        // Visual markers for tree structure
        var marker = isLast ? "└── " : "├── ";
        sb.AppendLine($"{indent}{marker}{node.Kind}");

        var newIndent = indent + (isLast ? "    " : "│   ");

        var children = node.ChildNodesAndTokens().ToArray();
        for (int i = 0; i < children.Length; i++)
        {
            var isChildLast = i == children.Length - 1;
            if (children[i].AsNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, sb, newIndent, isChildLast, includeTokens, includeTrivia, maxDepth, currentDepth + 1);
            }
            else if (includeTokens && children[i].AsToken(out var token))
            {
                // Print token
                sb.AppendLine($"{newIndent}{(isChildLast ? "└── " : "├── ")}{token.Kind}{(token.IsMissing ? " (Missing)" : "")} \"{token.Text}\"");

                // Include trivia if specified
                if (includeTrivia)
                {
                    // Leading trivia
                    foreach (var trivia in token.LeadingTrivia)
                    {
                        sb.AppendLine($"{newIndent}    [Leading Trivia] {trivia.Kind}: \"{TriviaToString(trivia)}\"");
                    }

                    // Trailing trivia
                    foreach (var trivia in token.TrailingTrivia)
                    {
                        sb.AppendLine($"{newIndent}    [Trailing Trivia] {trivia.Kind}: \"{TriviaToString(trivia)}\"");
                    }
                }
            }
        }
    }

    private static string TriviaToString(SyntaxTrivia trivia)
    {
        return trivia.ToString().Replace("\r", @"\r")
            .Replace("\n", @"\n")
            .Replace("\t", @"\t");
    }
}
