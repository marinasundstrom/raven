using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Text;

public class ColorScheme
{
    public AnsiColor Default { get; internal set; }

    public AnsiColor Method { get; internal set; }

    public AnsiColor Namespace { get; internal set; }

    public AnsiColor Keyword { get; internal set; }

    public AnsiColor StringLiteral { get; internal set; }

    public static ColorScheme Light { get; } = new ColorScheme()
    {
        Default = AnsiColor.BrightBlack,
        Method = AnsiColor.BrightRed,
        Namespace = AnsiColor.BrightCyan,
        Keyword = AnsiColor.BrightBlue,
        StringLiteral = AnsiColor.BrightYellow
    };

    public static ColorScheme Dark { get; } = new ColorScheme()
    {
        Default = AnsiColor.BrightWhite,
        Method = AnsiColor.BrightYellow,
        Namespace = AnsiColor.BrightCyan,
        Keyword = AnsiColor.BrightBlue,
        StringLiteral = AnsiColor.BrightRed
    };
}

public static class ConsoleSyntaxHighlighter
{
    public static ColorScheme ColorScheme { get; set; } = ColorScheme.Dark;

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
                    t = Colorize(t, ColorScheme.Keyword);
                }
                else if (token.Kind == SyntaxKind.StringLiteralToken)
                {
                    t = Colorize(t, ColorScheme.StringLiteral);
                }

                builder.Append(t);
            }
            else if (child.AsNode(out var childNode))
            {
                WriteNode(childNode, builder);
            }
        }
    }

    private static void WriteNode(SyntaxNode node, StringBuilder builder)
    {
        if (node is NameSyntax name)
        {
            WriteNameSyntax(name, builder);
        }
        else if (node is PredefinedTypeSyntax ptype)
        {
            string? t = ptype.ToFullString();
            t = Colorize(t, ColorScheme.Keyword);
            builder.Append(t);
        }
        else if (node is NameSyntax iname)
        {
            var symbol = SemanticModel.GetSymbolInfo(iname).Symbol;

            if (symbol is IMethodSymbol)
            {
                string? t = iname.ToFullString();
                t = Colorize(t, ColorScheme.Method);
                builder.Append(t);
            }
            else if (symbol is INamespaceSymbol or ITypeSymbol)
            {
                string? t = iname.ToFullString();
                t = Colorize(t, ColorScheme.Namespace);
                builder.Append(t);
            }
            else
            {
                string? t = iname.ToFullString();
                t = Colorize(t, ColorScheme.Default);
                builder.Append(t);
            }
        }
        else if (node is MemberAccessExpressionSyntax maccess)
        {
            WriteNode(maccess.Expression, builder);

            string? t = maccess.OperatorToken.ToFullString();
            t = Colorize(t, ColorScheme.Default);
            builder.Append(t);

            var symbol = SemanticModel.GetSymbolInfo(maccess).Symbol;

            var color = symbol is IMethodSymbol ? ColorScheme.Method : ColorScheme.Default;

            t = maccess.Name.ToFullString();
            t = Colorize(t, color);
            builder.Append(t);
        }
        else if (node is InvocationExpressionSyntax invocationExpression)
        {
            WriteNode(invocationExpression.Expression, builder);

            foreach (var x in invocationExpression.ChildNodes().Skip(1))
            {
                WriteNodeToText(x, builder);
            }
        }
        else
        {
            WriteNodeToText(node, builder);
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