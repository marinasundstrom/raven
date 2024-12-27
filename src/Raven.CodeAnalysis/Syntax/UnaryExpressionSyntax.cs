namespace Raven.CodeAnalysis.Syntax;

public partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OperatorToken { get; }

    public partial ExpressionSyntax Expression { get; }

    internal UnaryExpressionSyntax(
        InternalSyntax.UnaryExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public UnaryExpressionSyntax(SyntaxToken operatorToken, ExpressionSyntax expression)
          : this(
                new InternalSyntax.UnaryExpressionSyntax(operatorToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static UnaryExpressionSyntax UnaryExpression(SyntaxToken operatorToken, ExpressionSyntax expression)
        => new UnaryExpressionSyntax(operatorToken, expression);
}