namespace Raven.CodeAnalysis.Syntax;

public partial class SelfExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken SelfKeyword { get; }

    internal SelfExpressionSyntax(
        InternalSyntax.SelfExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public SelfExpressionSyntax(SyntaxToken selfKeyword)
          : this(
                new InternalSyntax.SelfExpressionSyntax(selfKeyword.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    /*public static SelfExpressionSyntax SelfExpression()
    => new SelfExpressionSyntax(SelfKeyword); */

    public static SelfExpressionSyntax SelfExpression(SyntaxToken selfKeyword)
        => new SelfExpressionSyntax(selfKeyword);
}