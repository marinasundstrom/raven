namespace Raven.CodeAnalysis.Syntax;

public partial class VariableDeclaratorSyntax : SyntaxNode
{
    public partial SyntaxToken Identifier { get; }
    public partial TypeAnnotationClauseSyntax TypeAnnotation { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    internal VariableDeclaratorSyntax(
        InternalSyntax.VariableDeclaratorSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public VariableDeclaratorSyntax(SyntaxToken identifier)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax(identifier.Green), null)
    {

    }

    public VariableDeclaratorSyntax(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotation)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax(identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax?)typeAnnotation.Green), null)
    {

    }

    public VariableDeclaratorSyntax(SyntaxToken identifier, EqualsValueClauseSyntax initializer)
      : this(
            new InternalSyntax.VariableDeclaratorSyntax(identifier.Green, (InternalSyntax.EqualsValueClauseSyntax)initializer?.Green), null)
    {

    }

    public VariableDeclaratorSyntax(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotation, EqualsValueClauseSyntax initializer)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax(identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax?)typeAnnotation?.Green, (InternalSyntax.EqualsValueClauseSyntax)initializer?.Green), null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier)
        => new VariableDeclaratorSyntax(identifier);

    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotationSyntax)
        => new VariableDeclaratorSyntax(identifier, typeAnnotationSyntax);

    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier, EqualsValueClauseSyntax initalizer)
        => new VariableDeclaratorSyntax(identifier, initalizer);

    public static VariableDeclaratorSyntax VariableDeclarator(SyntaxToken identifier, TypeAnnotationClauseSyntax typeAnnotationSyntax, EqualsValueClauseSyntax initalizer)
        => new VariableDeclaratorSyntax(identifier, typeAnnotationSyntax, initalizer);
}