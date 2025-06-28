namespace Raven.CodeAnalysis.Syntax;

public partial class TypeAnnotationClauseSyntax : SyntaxNode
{
    public partial SyntaxToken ColonToken { get; }
    public partial TypeSyntax Type { get; }

    internal TypeAnnotationClauseSyntax(
        InternalSyntax.TypeAnnotationClauseSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TypeAnnotationClauseSyntax(SyntaxToken colonToken, TypeSyntax type)
          : this(
                new InternalSyntax.TypeAnnotationClauseSyntax(colonToken.Green, (InternalSyntax.TypeSyntax)type.Green), null)
    {

    }

    public TypeAnnotationClauseSyntax(TypeSyntax type)
        : this(SyntaxFactory.ColonToken, type)
    {

    }
}

public static partial class SyntaxFactory
{
    public static TypeAnnotationClauseSyntax TypeAnnotation(SyntaxToken colonToken, TypeSyntax type)
        => new TypeAnnotationClauseSyntax(colonToken, type);

    public static TypeAnnotationClauseSyntax TypeAnnotation(TypeSyntax type)
         => new TypeAnnotationClauseSyntax(type);
}