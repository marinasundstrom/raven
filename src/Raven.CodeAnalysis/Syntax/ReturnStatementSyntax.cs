namespace Raven.CodeAnalysis.Syntax;

public partial class ReturnStatementSyntax : StatementSyntax
{
    public partial SyntaxToken ReturnKeyword { get; }

    public partial ExpressionSyntax? Expression { get; }

    public partial SyntaxToken TerminatorToken { get; }

    internal ReturnStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ReturnStatementSyntax(SyntaxToken returnKeyword, ExpressionSyntax? expression, SyntaxToken terminatorToken)
      : this(
            new InternalSyntax.ReturnStatementSyntax(returnKeyword.Green, (InternalSyntax.ExpressionSyntax)expression?.Green, terminatorToken.Green))
    {

    }

    public ReturnStatementSyntax(ExpressionSyntax? expression)
      : this(SyntaxFactory.ReturnKeyword, expression, SyntaxFactory.SemicolonToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ReturnStatementSyntax ReturnStatement()
     => new ReturnStatementSyntax(null);

    public static ReturnStatementSyntax ReturnStatement(ExpressionSyntax expression)
        => new ReturnStatementSyntax(expression);

    public static ReturnStatementSyntax ReturnStatement(SyntaxToken returnKeyword, ExpressionSyntax? expression, SyntaxToken terminatorToken)
        => new ReturnStatementSyntax(returnKeyword, expression, terminatorToken);

    public static ReturnStatementSyntax ReturnStatement(SyntaxToken returnKeyword, SyntaxToken terminatorToken)
        => new ReturnStatementSyntax(returnKeyword, null, terminatorToken);
}