namespace Raven.CodeAnalysis.Syntax;

public abstract partial class LiteralExpressionSyntax : ExpressionSyntax
{
    protected LiteralExpressionSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }
}

public partial class NumericLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public partial SyntaxToken NumberToken { get; }

    public NumericLiteralExpressionSyntax(
        InternalSyntax.NumericLiteralExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public NumericLiteralExpressionSyntax(SyntaxToken numberToken)
          : this(
                new InternalSyntax.NumericLiteralExpressionSyntax(numberToken.Green), null, 0)
    {

    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitNumericLiteralExpression(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitNumericLiteralExpression(this);
    }
}

public static partial class SyntaxFactory
{
    public static LiteralExpressionSyntax LiteralExpression(SyntaxToken numberToken) => new NumericLiteralExpressionSyntax(numberToken);

    public static LiteralExpressionSyntax LiteralExpression(int number) => LiteralExpression(NumericLiteral(number));
}