using System.Text;

namespace Raven.CodeAnalysis.Syntax;

/*
public static class SyntaxTreePrinter
{
    public static void PrintSyntaxTree(this SyntaxNode node, bool includeTokens = false)
    {
        StringBuilder sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, string.Empty);
        Console.WriteLine(sb.ToString(), includeTokens);
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, bool includeTokens = false)
    {
        StringBuilder sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, string.Empty, includeTokens);
        return sb.ToString();
    }

    private static void PrintSyntaxTreeCore(this SyntaxNode node, StringBuilder sb, string indent = "", bool includeTokens = false)
    {
        // Print the current node
        sb.AppendLine($"{indent}{node.Kind}");

        // Recursively print child nodes
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.AsNode(out var childNode))
            {
                PrintSyntaxTreeCore(childNode, sb, indent + "  ", includeTokens);
            }
            else if (includeTokens && child.AsToken(out var token))
            {
                sb.AppendLine($"{indent + "  "}{token.Kind}{(token.IsMissing ? " *" : string.Empty)}");
            }
        }
    }
}
*/