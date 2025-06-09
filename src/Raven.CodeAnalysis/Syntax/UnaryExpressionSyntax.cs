namespace Raven.CodeAnalysis.Syntax;

public partial class UnaryExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }

    public partial SyntaxToken OperatorToken { get; }

    public partial ExpressionSyntax Expression { get; }

    internal UnaryExpressionSyntax(
        InternalSyntax.UnaryExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public UnaryExpressionSyntax(SyntaxKind kind, SyntaxToken operatorToken, ExpressionSyntax expression)
          : this(
                new InternalSyntax.UnaryExpressionSyntax(kind, operatorToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static UnaryExpressionSyntax UnaryExpression(SyntaxKind kind, SyntaxToken operatorToken, ExpressionSyntax expression)
        => new UnaryExpressionSyntax(kind, operatorToken, expression);
}