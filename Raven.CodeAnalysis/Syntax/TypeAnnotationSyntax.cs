namespace Raven.CodeAnalysis.Syntax;

public partial class TypeAnnotationSyntax : SyntaxNode
{
    public partial SyntaxToken ColonToken { get; }
    public partial TypeSyntax Type { get; }

    internal TypeAnnotationSyntax(
        InternalSyntax.TypeAnnotationSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public TypeAnnotationSyntax(SyntaxToken colonToken, TypeSyntax type)
          : this(
                new InternalSyntax.TypeAnnotationSyntax(colonToken.Green, (InternalSyntax.TypeSyntax)type.Green), null)
    {

    }

    public TypeAnnotationSyntax(TypeSyntax type)
        : this(SyntaxFactory.ColonToken, type)
    {

    }
}

public static partial class SyntaxFactory
{
    public static TypeAnnotationSyntax TypeAnnotation(SyntaxToken colonToken, TypeSyntax type)
        => new TypeAnnotationSyntax(colonToken, type);

    public static TypeAnnotationSyntax TypeAnnotation(TypeSyntax type)
         => new TypeAnnotationSyntax(type);
}