namespace Raven.CodeAnalysis.Syntax;

public partial class ArrowExpressionClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ArrowToken { get; }

    public partial ExpressionSyntax Expression { get; }

    internal ArrowExpressionClauseSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ArrowExpressionClauseSyntax(
        SyntaxToken arrowToken,
        ExpressionSyntax expression)
        : this(new InternalSyntax.ArrowExpressionClauseSyntax(arrowToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArrowExpressionClauseSyntax ArrowExpressionClause(SyntaxToken arrowToken, ExpressionSyntax expression)
    => new ArrowExpressionClauseSyntax(arrowToken, expression);
}