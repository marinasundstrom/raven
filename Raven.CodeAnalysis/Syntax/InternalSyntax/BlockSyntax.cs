namespace Raven.CodeAnalysis.Syntax.InternalSyntax;


public class BlockSyntax : StatementSyntax
{
    public BlockSyntax(
        SyntaxToken openBraceToken,
        SyntaxList statements,
        SyntaxToken closeBraceToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      openBraceToken,
                      statements,
                      closeBraceToken
              ],
              openBraceToken.FullWidth + (statements?.FullWidth ?? 0) + closeBraceToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.BlockSyntax(this, parent);
    }
}