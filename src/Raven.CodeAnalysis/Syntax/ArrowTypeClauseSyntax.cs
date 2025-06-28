namespace Raven.CodeAnalysis.Syntax;

public partial class ArrowTypeClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ArrowToken { get; }
    public partial TypeSyntax Type { get; }

    internal ArrowTypeClauseSyntax(
        InternalSyntax.ArrowTypeClauseSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ArrowTypeClauseSyntax(SyntaxToken arrowToken, TypeSyntax type)
          : this(
                new InternalSyntax.ArrowTypeClauseSyntax(arrowToken.Green, (InternalSyntax.TypeSyntax)type.Green), null)
    {

    }

    public ArrowTypeClauseSyntax(TypeSyntax type)
        : this(SyntaxFactory.ArrowToken, type)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ArrowTypeClauseSyntax ReturnTypeAnnotation(SyntaxToken arrowToken, TypeSyntax type)
        => new ArrowTypeClauseSyntax(arrowToken, type);

    public static ArrowTypeClauseSyntax ReturnTypeAnnotation(TypeSyntax type)
         => new ArrowTypeClauseSyntax(type);
}