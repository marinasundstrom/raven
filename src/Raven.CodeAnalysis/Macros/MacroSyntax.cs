using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides concise inspection forms for syntax handled by macros.
/// </summary>
public static class MacroSyntax
{
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
}
