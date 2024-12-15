namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class LiteralExpressionSyntax : ExpressionSyntax
{
    protected LiteralExpressionSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}

public partial class NumericLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public NumericLiteralExpressionSyntax(
        SyntaxToken numberToken)
        : base(
              SyntaxKind.NumericLiteralExpression,
              [
                numberToken
              ])
    {
    }
}

public partial class BooleanLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public BooleanLiteralExpressionSyntax(
        SyntaxToken token)
        : base(
              SyntaxKind.BooleanLiteralExpression,
              [
                token
              ])
    {
    }
}