namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class BlockSyntax : StatementSyntax
{
    public BlockSyntax(
        SyntaxToken openBraceToken,
        SyntaxList statements,
        SyntaxToken closeBraceToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      openBraceToken ?? throw new ArgumentNullException(nameof(openBraceToken)),
                      statements ?? throw new ArgumentNullException(nameof(statements)),
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
    public static BlockSyntax Block(
        SyntaxToken openBraceToken,
        SyntaxList statements,
        SyntaxToken closeBraceToken,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(openBraceToken, statements, closeBraceToken, diagnostics);
}