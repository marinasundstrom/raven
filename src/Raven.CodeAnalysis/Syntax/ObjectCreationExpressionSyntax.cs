namespace Raven.CodeAnalysis.Syntax;

public partial class ObjectCreationExpressionSyntax : ExpressionSyntax
{
    public partial SyntaxToken NewKeyword { get; }

    public partial TypeSyntax Type { get; }

    public partial ArgumentListSyntax ArgumentList { get; }

    internal ObjectCreationExpressionSyntax(
        InternalSyntax.SyntaxNode greenNode,
        SyntaxNode parent = null,
        int position = 0)
    : base(greenNode, parent, position)
    {
    }

    public ObjectCreationExpressionSyntax(SyntaxToken newKeyword, TypeSyntax? type, ArgumentListSyntax argumentList)
      : this(
            new InternalSyntax.ObjectCreationExpressionSyntax(newKeyword.Green, (InternalSyntax.TypeSyntax)type!.Green, (InternalSyntax.ArgumentListSyntax)argumentList.Green))
    {

    }

    public ObjectCreationExpressionSyntax(TypeSyntax? type, ArgumentListSyntax argumentList)
      : this(SyntaxFactory.NewKeyword, type, argumentList)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ObjectCreationExpressionSyntax ObjectCreationExpression(SyntaxToken newKeyword, TypeSyntax type, ArgumentListSyntax argumentList)
        => new ObjectCreationExpressionSyntax(newKeyword, type, argumentList);
}