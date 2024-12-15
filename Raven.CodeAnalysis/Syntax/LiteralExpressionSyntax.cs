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
}


public partial class BooleanLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public partial SyntaxToken NumberToken { get; }

    public BooleanLiteralExpressionSyntax(
        InternalSyntax.BooleanLiteralExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public BooleanLiteralExpressionSyntax(SyntaxToken token)
          : this(
                new InternalSyntax.BooleanLiteralExpressionSyntax(token.Green), null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static NumericLiteralExpressionSyntax LiteralExpression(SyntaxToken numberToken) => new NumericLiteralExpressionSyntax(numberToken);

    public static LiteralExpressionSyntax LiteralExpression(int number) => LiteralExpression(NumericLiteral(number));

    public static BooleanLiteralExpressionSyntax BooleanLiteralExpression(SyntaxToken booleanLiteralToken) => new BooleanLiteralExpressionSyntax(booleanLiteralToken);
}