namespace Raven.CodeAnalysis.Syntax;

public partial class BinaryExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial ExpressionSyntax LeftHandSide { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial ExpressionSyntax RightHandSide { get; }

    internal BinaryExpressionSyntax(
        InternalSyntax.BinaryExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BinaryExpressionSyntax(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.BinaryExpressionSyntax(kind, (InternalSyntax.ExpressionSyntax)leftHandSide.Green, operatorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static BinaryExpressionSyntax BinaryExpression(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new BinaryExpressionSyntax(kind, leftHandSide, operatorToken, rightHandSize);
}