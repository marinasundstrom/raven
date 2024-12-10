namespace Raven.CodeAnalysis.Syntax;

public partial class BinaryExpressionSyntax : ExpressionSyntax
{
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

    public BinaryExpressionSyntax(ExpressionSyntax leftHandSide, SyntaxToken opratorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.BinaryExpressionSyntax((InternalSyntax.ExpressionSyntax)leftHandSide.Green, opratorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitBinaryExpression(this);
    }
}

public static partial class SyntaxFactory
{
    public static BinaryExpressionSyntax BinaryExpression(ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new BinaryExpressionSyntax(leftHandSide, operatorToken, rightHandSize);
}