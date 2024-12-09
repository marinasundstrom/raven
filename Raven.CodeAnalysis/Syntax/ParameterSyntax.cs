namespace Raven.CodeAnalysis.Syntax;

public partial class ParameterSyntax : SyntaxNode
{
    public partial IdentifierNameSyntax Name { get; }

    public partial TypeAnnotationSyntax? TypeAnnotation { get; }

    public ParameterSyntax(GreenNode greenNode, SyntaxNode parent)
        : base(greenNode, parent)
    {
    }

    public ParameterSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax? typeAnnotation)
        : this(new InternalSyntax.ParameterSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.TypeAnnotationSyntax?)typeAnnotation?.Green), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(IdentifierNameSyntax name)
        => new ParameterSyntax(name, null);

    public static ParameterSyntax Parameter(IdentifierNameSyntax name, TypeAnnotationSyntax? typeAnnotation)
        => new ParameterSyntax(name, typeAnnotation);
}