namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class BlockSyntax : StatementSyntax
{
    public BlockSyntax(
        SyntaxToken openBraceToken,
        SyntaxList statements,
        SyntaxToken closeBraceToken)
        : base(
              SyntaxKind.Block,
              [
                      openBraceToken,
                      statements,
                      closeBraceToken
              ])
    {
    }
}