namespace Raven.CodeAnalysis.Syntax;

public partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial ExpressionSyntax LeftHandSide { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial ExpressionSyntax RightHandSide { get; }

    public BinaryExpressionSyntax(
        InternalSyntax.BinaryExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BinaryExpressionSyntax(ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSide)
      : this(GetKind(operatorToken), leftHandSide, operatorToken, rightHandSide)
    {

    }

    internal BinaryExpressionSyntax(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.BinaryExpressionSyntax(kind, (InternalSyntax.ExpressionSyntax)leftHandSide.Green, operatorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }

    public static SyntaxKind GetKind(SyntaxToken operatorToken)
    {
        switch (operatorToken.Kind)
        {
            case SyntaxKind.PlusToken:
                return SyntaxKind.AddExpression;

            case SyntaxKind.MinusToken:
                return SyntaxKind.SubtractExpression;

            case SyntaxKind.StarToken:
                return SyntaxKind.MultiplyExpression;

            case SyntaxKind.SlashToken:
                return SyntaxKind.DivideExpression;

            case SyntaxKind.PercentToken:
                return SyntaxKind.ModuloExpression;

            case SyntaxKind.EqualsToken:
                return SyntaxKind.EqualsExpression;

            case SyntaxKind.NotEqualsToken:
                return SyntaxKind.NotEqualsExpression;

            case SyntaxKind.LessThanEqualsToken:
                return SyntaxKind.LessThanExpression;

            case SyntaxKind.GreaterThanToken:
                return SyntaxKind.GreaterThanExpression;

            case SyntaxKind.LessThanOrEqualExpression:
                return SyntaxKind.LessThanOrEqualExpression;

            case SyntaxKind.GreaterOrEqualsToken:
                return SyntaxKind.GreaterThanOrEqualExpression;
        }

        throw new ArgumentException("Kind is not valid for this expression.");
    }
}

public static partial class SyntaxFactory
{
    public static BinaryExpressionSyntax BinaryExpression(ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new BinaryExpressionSyntax(leftHandSide, operatorToken, rightHandSize);
}