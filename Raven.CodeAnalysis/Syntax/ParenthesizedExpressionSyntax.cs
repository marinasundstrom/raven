namespace Raven.CodeAnalysis.Syntax;

public partial class ParenthesizedExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenParenToken { get; }

    public partial ExpressionSyntax Expression { get; }

    public partial SyntaxToken CloseParenToken { get; }

    public ParenthesizedExpressionSyntax(
        InternalSyntax.ParenthesizedExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ParenthesizedExpressionSyntax(SyntaxToken openParenToken, ExpressionSyntax expression, SyntaxToken closeParenToken)
          : this(
                new InternalSyntax.ParenthesizedExpressionSyntax(openParenToken.Green, (InternalSyntax.ExpressionSyntax)expression.Green, closeParenToken.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParenthesizedExpressionSyntax ParenthesizedExpression(SyntaxToken openParenToken, ExpressionSyntax expression, SyntaxToken closeParenToken)
        => new ParenthesizedExpressionSyntax(openParenToken, expression, closeParenToken);
}