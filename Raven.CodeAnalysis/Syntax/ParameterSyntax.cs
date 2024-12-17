namespace Raven.CodeAnalysis.Syntax;

public partial class ParameterSyntax : SyntaxNode
{
    public partial IdentifierNameSyntax Name { get; }

    public partial TypeAnnotationSyntax? TypeAnnotation { get; }

    internal ParameterSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ParameterSyntax(
        IdentifierNameSyntax name,
        TypeAnnotationSyntax? typeAnnotation)
        : this(new InternalSyntax.ParameterSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.TypeAnnotationSyntax?)typeAnnotation?.Green), (SyntaxNode)null, 0)
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