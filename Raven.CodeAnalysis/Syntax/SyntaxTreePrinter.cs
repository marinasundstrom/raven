using System.Text;

namespace Raven.CodeAnalysis.Syntax;

public static class SyntaxTreePrinter
{
    public static void PrintSyntaxTree(this SyntaxNode node, string indent = "")
    {
        StringBuilder sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, indent);
        Console.WriteLine(sb.ToString());
    }

    public static string GetSyntaxTreeRepresentation(this SyntaxNode node, string indent = "")
    {
        StringBuilder sb = new StringBuilder();
        PrintSyntaxTreeCore(node, sb, indent);
        return sb.ToString();
    }

    private static void PrintSyntaxTreeCore(this SyntaxNode node, StringBuilder sb, string indent = "")
    {
        // Print the current node
        sb.AppendLine($"{indent}{node.Kind}");

        // Recursively print child nodes
        foreach (var child in node.ChildNodes())
        {
            PrintSyntaxTreeCore(child, sb, indent + "  ");
        }
    }
}