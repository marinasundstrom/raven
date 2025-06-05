namespace Raven.CodeAnalysis.Syntax;

public partial class ParameterSyntax : SyntaxNode
{
    public partial SyntaxToken Identifier { get; }

    public partial TypeAnnotationSyntax? TypeAnnotation { get; }

    internal ParameterSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ParameterSyntax(
        SyntaxToken identifier,
        TypeAnnotationSyntax? typeAnnotation)
        : this(new InternalSyntax.ParameterSyntax(identifier.Green, (InternalSyntax.TypeAnnotationSyntax?)typeAnnotation?.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(SyntaxToken identifier)
        => new ParameterSyntax(identifier, null);

    public static ParameterSyntax Parameter(SyntaxToken identifier, TypeAnnotationSyntax? typeAnnotation)
        => new ParameterSyntax(identifier, typeAnnotation);
}