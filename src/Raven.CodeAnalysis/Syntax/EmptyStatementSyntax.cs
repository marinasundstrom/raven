namespace Raven.CodeAnalysis.Syntax;

public partial class EmptyStatementSyntax : StatementSyntax
{
    public partial SyntaxToken TerminatorToken { get; }

    internal EmptyStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public EmptyStatementSyntax(SyntaxToken terminatorToken)
      : this(
            new InternalSyntax.EmptyStatementSyntax(terminatorToken.Green))
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

    public static EmptyStatementSyntax EmptyStatement(SyntaxToken terminatorToken)
        => new EmptyStatementSyntax(terminatorToken);
}