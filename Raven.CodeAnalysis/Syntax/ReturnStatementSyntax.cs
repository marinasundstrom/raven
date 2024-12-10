namespace Raven.CodeAnalysis.Syntax;

public partial class ReturnStatementSyntax : StatementSyntax
{
    public partial SyntaxToken ReturnKeyword { get; }

    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken SemicolonToken { get; }

    public ReturnStatementSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ReturnStatementSyntax(SyntaxToken returnKeyword, ExpressionSyntax expression, SyntaxToken semicolonToken)
      : this(
            new InternalSyntax.ReturnStatementSyntax(returnKeyword.Green, (InternalSyntax.ExpressionSyntax)expression.Green, semicolonToken.Green))
    {

    }

    public ReturnStatementSyntax(ExpressionSyntax expression)
      : this(SyntaxFactory.ReturnKeyword, expression, SyntaxFactory.SemicolonToken)
    {

    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitReturnStatement(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitReturnStatement(this);
    }

}

public static partial class SyntaxFactory
{
    public static ReturnStatementSyntax ReturnStatement(ExpressionSyntax expression)
        => new ReturnStatementSyntax(expression);

    public static ReturnStatementSyntax ReturnStatement(SyntaxToken returnKeyword, ExpressionSyntax expression, SyntaxToken semicolonToken)
        => new ReturnStatementSyntax(returnKeyword, expression, semicolonToken);
}