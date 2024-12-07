namespace Raven.CodeAnalysis.Syntax;

public partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public partial ExpressionSyntax LeftHandSide { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial ExpressionSyntax RightHandSide { get; }

    public BinaryExpressionSyntax(
        InternalSyntax.BinaryExpressionSyntax greenNode,
        SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public BinaryExpressionSyntax(ExpressionSyntax leftHandSide, SyntaxToken opratorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.BinaryExpressionSyntax((InternalSyntax.ExpressionSyntax)leftHandSide.Green, opratorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static BinaryExpressionSyntax BinaryExpression(ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new BinaryExpressionSyntax(leftHandSide, operatorToken, rightHandSize);
}