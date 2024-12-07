namespace Raven.CodeAnalysis.Syntax;

public partial class ReturnStatementSyntax : StatementSyntax
{
    public partial SyntaxToken ReturnKeyword { get; }

    public partial ExpressionSyntax Expression { get; }

    public ReturnStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null)
    : base(greenNode, parent)
    {
    }

    public ReturnStatementSyntax(SyntaxToken returnKeyword, ExpressionSyntax expression)
      : this(
            new InternalSyntax.ReturnStatementSyntax(returnKeyword.Green, (InternalSyntax.ExpressionSyntax)expression.Green))
    {

    }

    public ReturnStatementSyntax(ExpressionSyntax expression)
      : this(SyntaxFactory.ReturnKeyword, expression)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ReturnStatementSyntax ReturnStatement(ExpressionSyntax expression)
        => new ReturnStatementSyntax(expression);
}