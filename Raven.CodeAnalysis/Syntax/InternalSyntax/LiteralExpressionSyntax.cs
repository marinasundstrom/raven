namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class LiteralExpressionSyntax : ExpressionSyntax
{
    protected LiteralExpressionSyntax(SyntaxKind kind, GreenNode[] slots)
        : base(kind, slots)
    {
    }
}

public class NumericLiteralExpressionSyntax : LiteralExpressionSyntax
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

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.NumericLiteralExpressionSyntax(this, parent, position);
    }
}