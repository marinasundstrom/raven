namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class TypeArgumentListSyntax : SyntaxNode
{
    public TypeArgumentListSyntax(
        SyntaxToken lessThanToken,
        SyntaxList arguments,
        SyntaxToken greaterThanToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
            SyntaxKind.TypeArgumentList,
            [
                lessThanToken,
                arguments,
                greaterThanToken
            ],
            diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static TypeArgumentListSyntax TypeArgumentList(
        SyntaxToken lessThanToken,
        SyntaxList arguments,
        SyntaxToken greaterThanToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(lessThanToken, arguments, greaterThanToken, diagnostics);
}
