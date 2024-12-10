namespace Raven.CodeAnalysis.Syntax;

public partial class ExpressionStatementSyntax : StatementSyntax
{
    public partial ExpressionSyntax Expression { get; }

    public ExpressionStatementSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ExpressionStatementSyntax(ExpressionSyntax expression)
        : this(new InternalSyntax.ExpressionStatementSyntax((InternalSyntax.ExpressionSyntax)expression.Green))
    {
    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitExpressionStatement(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitExpressionStatement(this);
    }
}

public static partial class SyntaxFactory
{
    public static ExpressionStatementSyntax ExpressionStatement(ExpressionSyntax expression)
        => new ExpressionStatementSyntax(expression);
}