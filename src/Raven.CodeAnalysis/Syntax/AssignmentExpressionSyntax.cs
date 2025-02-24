namespace Raven.CodeAnalysis.Syntax;

public partial class AssignmentExpressionSyntax : ExpressionSyntax
{
    public override partial SyntaxKind Kind { get; }
    public partial ExpressionSyntax LeftHandSide { get; }
    public partial SyntaxToken OperatorToken { get; }
    public partial ExpressionSyntax RightHandSide { get; }

    internal AssignmentExpressionSyntax(
        InternalSyntax.AssignmentExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public AssignmentExpressionSyntax(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSide)
          : this(
                new InternalSyntax.AssignmentExpressionSyntax(kind, (InternalSyntax.ExpressionSyntax)leftHandSide.Green, operatorToken.Green, (InternalSyntax.ExpressionSyntax)rightHandSide.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static AssignmentExpressionSyntax AssignmentExpression(SyntaxKind kind, ExpressionSyntax leftHandSide, SyntaxToken operatorToken, ExpressionSyntax rightHandSize)
        => new AssignmentExpressionSyntax(kind, leftHandSide, operatorToken, rightHandSize);
}