namespace Raven.CodeAnalysis.Syntax;

public partial class BlockSyntax : StatementSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList<StatementSyntax> Statements { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    public BlockSyntax(
        InternalSyntax.BlockSyntax greenNode,
        SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public BlockSyntax(SyntaxToken openBraceToken, SyntaxList<StatementSyntax> statements, SyntaxToken closeBraceToken)
          : this(
                new InternalSyntax.BlockSyntax(openBraceToken.Green, statements.Green, closeBraceToken.Green), null)
    {

    }

    public BlockSyntax(SyntaxList<StatementSyntax> statements)
        : this(SyntaxFactory.OpenBraceToken, statements, SyntaxFactory.CloseBraceToken)
    {

    }

    // Additional properties or methods specific to BlockSyntax can be added here.
}

public static partial class SyntaxFactory
{
    public static BlockSyntax Block(SyntaxList<StatementSyntax> statements)
        => new BlockSyntax(statements);
}
