namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide)
        : base(
              SyntaxKind.BinaryExpression,
              [
                      leftHandSide,
                      operatorToken,
                      rightHandSide
              ])
    {

    }

    public override SyntaxKind Kind
    {
        get
        {
            var slot = GetSlot(1);
            switch (slot.Kind)
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

            throw new Exception();
        }
    }
}
