
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ImportDirectiveSyntax : SyntaxNode
{
    public ImportDirectiveSyntax(
        SyntaxToken importKeyword,
        TypeSyntax namespaceOrType,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
            SyntaxKind.ImportDirective,
            [
                importKeyword ?? throw new ArgumentNullException(nameof(importKeyword)),
                namespaceOrType ?? throw new ArgumentNullException(nameof(namespaceOrType)),
                semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
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
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(importKeyword, @namespace, semicolonToken, diagnostics);
}