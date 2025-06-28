namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class AccessorListSyntax : ExpressionSyntax
{
    public AccessorListSyntax(
        SyntaxToken openBraceToken,
        SyntaxList accessors,
        SyntaxToken closeBraceToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.AccessorList,
              [
                      openBraceToken ?? throw new ArgumentNullException(nameof(openBraceToken)),
                      accessors ?? throw new ArgumentNullException(nameof(accessors)),
                      closeBraceToken ?? throw new ArgumentNullException(nameof(closeBraceToken))
              ],
              diagnostics)
    {
        if (openBraceToken.Kind != SyntaxKind.OpenBraceToken)
            throw new ArgumentException("Invalid token for open brace.", nameof(openBraceToken));

        if (closeBraceToken.Kind != SyntaxKind.CloseBraceToken)
            throw new ArgumentException("Invalid token for close brace.", nameof(closeBraceToken));
    }
}

internal static partial class SyntaxFactory
{
    public static AccessorListSyntax AccessorList(
        SyntaxToken openBraceToken,
        SyntaxList accessors,
        SyntaxToken closeBraceToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(openBraceToken, accessors, closeBraceToken, diagnostics);
}