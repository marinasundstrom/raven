namespace Raven.CodeAnalysis.Syntax;

public partial class ParameterSyntax : SyntaxNode
{
    public partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken Identifier { get; }

    public partial TypeAnnotationSyntax? TypeAnnotation { get; }

    internal ParameterSyntax(GreenNode greenNode, SyntaxNode parent, int position)
        : base(greenNode, parent, position)
    {
    }

    public ParameterSyntax(
        SyntaxTokenList modifiers,
        SyntaxToken identifier,
        TypeAnnotationSyntax? typeAnnotation)
        : this(new InternalSyntax.ParameterSyntax(modifiers.Green, identifier.Green, (InternalSyntax.TypeAnnotationSyntax?)typeAnnotation?.Green), (SyntaxNode)null, 0)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ParameterSyntax Parameter(
        SyntaxTokenList modifiers,
        SyntaxToken identifier)
        => new ParameterSyntax(modifiers, identifier, null);
    
    public static ParameterSyntax Parameter(
        SyntaxToken identifier)
        => new ParameterSyntax(SyntaxTokenList.Empty, identifier, null);

    public static ParameterSyntax Parameter(
        SyntaxTokenList modifiers,
        SyntaxToken identifier, 
        TypeAnnotationSyntax? typeAnnotation)
        => new ParameterSyntax(modifiers, identifier, typeAnnotation);
    
    public static ParameterSyntax Parameter(
        SyntaxToken identifier, 
        TypeAnnotationSyntax? typeAnnotation)
        => new ParameterSyntax(SyntaxTokenList.Empty, identifier, typeAnnotation);
}