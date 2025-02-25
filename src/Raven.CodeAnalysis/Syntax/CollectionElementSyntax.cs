namespace Raven.CodeAnalysis.Syntax;

public partial class CollectionElementSyntax : SyntaxNode
{
    public partial ExpressionSyntax Expression { get; }

    internal CollectionElementSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public CollectionElementSyntax(
        ExpressionSyntax expression)
        : this(new InternalSyntax.CollectionElementSyntax((InternalSyntax.ExpressionSyntax)expression.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static CollectionElementSyntax CollectionElement(ExpressionSyntax expression)
        => new CollectionElementSyntax(expression);
}