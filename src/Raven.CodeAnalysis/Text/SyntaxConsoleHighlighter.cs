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
                if (childNode is NameSyntax name)
                {
                    WriteNameSyntax(name, builder);
                }
                else if (childNode is IdentifierNameSyntax iname)
                {
                    var symbol = SemanticModel.GetSymbolInfo(iname).Symbol;

                    if (symbol is IMethodSymbol)
                    {
                        string? t = iname.ToFullString();
                        t = Colorize(t, AnsiColor.BrightRed);
                        builder.Append(t);
                    }
                    else if (symbol is INamespaceSymbol or ITypeSymbol)
                    {
                        string? t = iname.ToFullString();
                        t = Colorize(t, AnsiColor.BrightCyan);
                        builder.Append(t);
                    }
                    else
                    {
                        string? t = iname.ToFullString();
                        t = Colorize(t, AnsiColor.BrightBlack);
                        builder.Append(t);
                    }
                }
                else if (childNode is MemberAccessExpressionSyntax maccess)
                {
                    WriteNodeToText(maccess.Expression, builder);

                    string? t = maccess.OperatorToken.ToFullString();
                    t = Colorize(t, AnsiColor.BrightBlack);
                    builder.Append(t);

                    t = maccess.Name.ToFullString();
                    t = Colorize(t, AnsiColor.BrightRed);
                    builder.Append(t);
                }
                else
                {
                    WriteNodeToText(childNode, builder);
                }
            }
        }
    }

    private static void WriteNameSyntax(NameSyntax name, StringBuilder builder)
    {
        if (name is IdentifierNameSyntax iname)
        {
            var symbol = SemanticModel.GetSymbolInfo(iname).Symbol;

            if (symbol is IMethodSymbol)
            {
                string? t = iname.ToFullString();
                t = Colorize(t, AnsiColor.BrightRed);
                builder.Append(t);
            }
            else if (symbol is INamespaceSymbol or ITypeSymbol)
            {
                string? t = iname.ToFullString();
                t = Colorize(t, AnsiColor.BrightCyan);
                builder.Append(t);
            }
            else
            {
                string? t = iname.ToFullString();
                t = Colorize(t, AnsiColor.White);
                builder.Append(t);
            }
        }
        else if (name is QualifiedNameSyntax qname)
        {
            WriteNameSyntax(qname.Left, builder);

            WriteNameSyntax(qname.Right, builder);
        }
    }

    private static string Colorize(string text, AnsiColor color)
    {
        return $"\u001b[{(int)color}m{text}\u001b[{(int)AnsiColor.Reset}m";
    }
}