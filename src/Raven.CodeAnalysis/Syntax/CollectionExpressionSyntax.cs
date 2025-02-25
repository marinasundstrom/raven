namespace Raven.CodeAnalysis.Syntax;

public partial class CollectionExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken OpenBracketToken { get; }
    public partial SeparatedSyntaxList<CollectionElementSyntax> Elements { get; }
    public partial SyntaxToken CloseBracketToken { get; }

    internal CollectionExpressionSyntax(

        InternalSyntax.CollectionExpressionSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public CollectionExpressionSyntax(SyntaxToken openBracketToken, SeparatedSyntaxList<CollectionElementSyntax> elements, SyntaxToken closeBraceToken)
          : this(
                new InternalSyntax.CollectionExpressionSyntax(openBracketToken.Green, elements.Green, closeBraceToken.Green), null)
    {

    }

    public CollectionExpressionSyntax(SeparatedSyntaxList<CollectionElementSyntax> elements)
        : this(SyntaxFactory.OpenBraceToken, elements, SyntaxFactory.CloseBraceToken)
    {

    }
}

public static partial class SyntaxFactory
{
    public static CollectionExpressionSyntax CollectionExpression()
    => new CollectionExpressionSyntax(SeparatedSyntaxList<CollectionElementSyntax>.Empty);

    public static CollectionExpressionSyntax CollectionExpression(SeparatedSyntaxList<CollectionElementSyntax> elements)
        => new CollectionExpressionSyntax(elements);

    public static CollectionExpressionSyntax CollectionExpression(SyntaxToken openBracketToken, SeparatedSyntaxList<CollectionElementSyntax> elements, SyntaxToken closeBracketToken)
        => new CollectionExpressionSyntax(openBracketToken, elements, closeBracketToken);
}