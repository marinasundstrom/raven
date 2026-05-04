namespace Raven.CodeAnalysis.Syntax;

public sealed partial class TrailingBlockExpressionSyntax
{
    public BlockSyntax Body => SyntaxFactory.Block(OpenBraceToken, Statements, CloseBraceToken);
}
