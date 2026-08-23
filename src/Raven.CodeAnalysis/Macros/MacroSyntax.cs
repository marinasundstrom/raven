using System.Text;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides concise inspection forms for syntax handled by macros.
/// </summary>
public static class MacroSyntax
{
    /// <summary>
    /// Creates a Raven string-literal expression whose source text safely
    /// represents <paramref name="value"/>.
    /// </summary>
    public static LiteralExpressionSyntax StringLiteral(string value)
    {
        ArgumentNullException.ThrowIfNull(value);
        return SyntaxFactory.LiteralExpression(
            SyntaxKind.StringLiteralExpression,
            SyntaxFactory.Literal(FormatStringLiteral(value), value));
    }

    public static string GetStructure(
        SyntaxNode syntax,
        PrinterOptions? options = null)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        options ??= new PrinterOptions
        {
            Colorize = false,
            IncludeNames = true,
        };
        return syntax.GetSyntaxTreeRepresentation(options);
    }

    public static string GetFactoryForm(
        SyntaxNode syntax,
        RavenQuoterOptions? options = null)
    {
        ArgumentNullException.ThrowIfNull(syntax);
        return RavenQuoter.Quote(syntax, options);
    }

    private static string FormatStringLiteral(string value)
    {
        var builder = new StringBuilder(value.Length + 2);
        builder.Append('"');
        foreach (var character in value)
        {
            switch (character)
            {
                case '\\':
                    builder.Append(@"\\");
                    break;
                case '"':
                    builder.Append("\\\"");
                    break;
                case '\0':
                    builder.Append(@"\0");
                    break;
                case '\a':
                    builder.Append(@"\a");
                    break;
                case '\b':
                    builder.Append(@"\b");
                    break;
                case '\f':
                    builder.Append(@"\f");
                    break;
                case '\n':
                    builder.Append(@"\n");
                    break;
                case '\r':
                    builder.Append(@"\r");
                    break;
                case '\t':
                    builder.Append(@"\t");
                    break;
                case '\v':
                    builder.Append(@"\v");
                    break;
                default:
                    if (char.IsControl(character))
                        builder.Append(@"\u").Append(((int)character).ToString("X4"));
                    else
                        builder.Append(character);
                    break;
            }
        }

        return builder.Append('"').ToString();
    }
}
