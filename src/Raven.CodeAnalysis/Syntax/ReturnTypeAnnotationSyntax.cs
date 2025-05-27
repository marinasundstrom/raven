namespace Raven.CodeAnalysis.Syntax;

public partial class ReturnTypeAnnotationSyntax : SyntaxNode
{
    public partial SyntaxToken ArrowToken { get; }
    public partial TypeSyntax Type { get; }

    internal ReturnTypeAnnotationSyntax(
        InternalSyntax.ReturnTypeAnnotationSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ReturnTypeAnnotationSyntax(SyntaxToken arrowToken, TypeSyntax type)
          : this(
                new InternalSyntax.ReturnTypeAnnotationSyntax(arrowToken.Green, (InternalSyntax.TypeSyntax)type.Green), null)
    {

    }

    public ReturnTypeAnnotationSyntax(TypeSyntax type)
        : this(SyntaxFactory.ArrowToken, type)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ReturnTypeAnnotationSyntax ReturnTypeAnnotation(SyntaxToken arrowToken, TypeSyntax type)
        => new ReturnTypeAnnotationSyntax(arrowToken, type);

    public static ReturnTypeAnnotationSyntax ReturnTypeAnnotation(TypeSyntax type)
         => new ReturnTypeAnnotationSyntax(type);
}