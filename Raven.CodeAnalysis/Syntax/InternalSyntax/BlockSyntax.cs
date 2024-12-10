namespace Raven.CodeAnalysis.Syntax.InternalSyntax;


public class BlockSyntax : StatementSyntax
{
    public BlockSyntax(
        SyntaxToken openBraceToken,
        SyntaxList statements,
        SyntaxToken closeBraceToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      openBraceToken,
                      statements,
                      closeBraceToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.BlockSyntax(this, parent, position);
    }
}