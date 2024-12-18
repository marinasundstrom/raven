namespace Raven.CodeAnalysis.Syntax;

public partial class EmptyStatementSyntax : StatementSyntax
{
    public partial SyntaxToken SemicolonToken { get; }

    internal EmptyStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public EmptyStatementSyntax(SyntaxToken semicolonToken)
      : this(
            new InternalSyntax.EmptyStatementSyntax(semicolonToken.Green))
    {

    }

    public EmptyStatementSyntax()
      : this(SyntaxFactory.SemicolonToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static EmptyStatementSyntax EmptyStatement()
        => new EmptyStatementSyntax();

    public static EmptyStatementSyntax EmptyStatement(SyntaxToken semicolonToken)
        => new EmptyStatementSyntax(semicolonToken);
}