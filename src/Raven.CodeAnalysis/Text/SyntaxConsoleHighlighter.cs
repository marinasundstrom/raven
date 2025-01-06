using System.Diagnostics.SymbolStore;
using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public static class SyntaxConsoleHighlighter
{
    public static Compilation Compilation { get; private set; }
    public static SemanticModel SemanticModel { get; private set; }

    public static string WriteNodeToText(this SyntaxNode node, Compilation compilation)
    {
        Compilation = compilation;
        SemanticModel = compilation.GetSemanticModel(node.SyntaxTree);

        var builder = new StringBuilder();

        WriteNodeToText(node, builder);
        return builder.ToString();
    }

    private static void WriteNodeToText(SyntaxNode node, StringBuilder builder)
    {
        if (node is null)
            return;

        // Recursively write child nodes
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.AsToken(out var token))
            {
                string? t = token.ToFullString();

                if (SyntaxFacts.IsKeywordKind(token.Kind))
                {
                    t = Colorize(t, AnsiColor.BrightBlue);
                }
                else if (token.Kind == SyntaxKind.StringLiteralToken)
                {
                    t = Colorize(t, AnsiColor.BrightYellow);
                }

                builder.Append(t);
            }
            else if (child.AsNode(out var childNode))
            {
                WriteNodeToText(childNode, builder);

                /*
                var symbol = SemanticModel.GetSymbolInfo(childNode);

                if(symbol is ISymbolMethod) 
                {
                    WriteNodeToText(childNode, builder);
                }
                else 
                {
                    WriteNodeToText(childNode, builder);
                }
                */
            }
        }
    }

    private static string Colorize(string text, AnsiColor color)
    {
        return $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";
    }
}
