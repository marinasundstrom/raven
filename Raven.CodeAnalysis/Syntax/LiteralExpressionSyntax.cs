namespace Raven.CodeAnalysis.Syntax;

public abstract partial class LiteralExpressionSyntax : ExpressionSyntax
{
    protected LiteralExpressionSyntax(GreenNode greenNode, SyntaxNode parent) : base(greenNode, parent)
    {
    }

    protected LiteralExpressionSyntax(GreenNode greenNode, SyntaxTree syntaxTree) : base(greenNode, syntaxTree)
    {
    }
}

public partial class NumericLiteralExpressionSyntax : LiteralExpressionSyntax
{
    public partial SyntaxToken NumberToken { get; }

    public NumericLiteralExpressionSyntax(
        InternalSyntax.NumericLiteralExpressionSyntax greenNode,
        SyntaxNode parent = null)
        : base(greenNode, parent)
    {
    }

    public NumericLiteralExpressionSyntax(SyntaxToken numberToken)
          : this(
                new InternalSyntax.NumericLiteralExpressionSyntax(numberToken.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static LiteralExpressionSyntax LiteralExpression(SyntaxToken numberToken) => new NumericLiteralExpressionSyntax(numberToken);

    public static LiteralExpressionSyntax LiteralExpression(int number) => LiteralExpression(NumericLiteral(number));
}