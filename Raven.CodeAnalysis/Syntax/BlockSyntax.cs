namespace Raven.CodeAnalysis.Syntax;

public partial class BlockSyntax : StatementSyntax
{
    public partial SyntaxToken OpenBraceToken { get; }
    public partial SyntaxList<StatementSyntax> Statements { get; }
    public partial SyntaxToken CloseBraceToken { get; }

    public BlockSyntax(
        InternalSyntax.BlockSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
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

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitBlock(this);
    }
}

public static partial class SyntaxFactory
{
    public static BlockSyntax Block(SyntaxList<StatementSyntax> statements)
        => new BlockSyntax(statements);

    public static BlockSyntax Block(SyntaxToken openBraceToken, SyntaxList<StatementSyntax> statements, SyntaxToken closeBraceToken)
        => new BlockSyntax(openBraceToken, statements, closeBraceToken);
}