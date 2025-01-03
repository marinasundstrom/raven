
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ImportDirectiveSyntax : SyntaxNode
{
    public ImportDirectiveSyntax(
        SyntaxToken importKeyword,
        NameSyntax @namespace,
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
            SyntaxKind.ImportDirective,
            [
                importKeyword,
                @namespace,
                semicolonToken
            ],
            diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(
        SyntaxToken importKeyword,
        NameSyntax @namespace,
        SyntaxToken semicolonToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(importKeyword, @namespace, semicolonToken, diagnostics);
}