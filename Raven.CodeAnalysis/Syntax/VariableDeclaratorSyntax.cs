namespace Raven.CodeAnalysis.Syntax;

public partial class VariableDeclaratorSyntax : SyntaxNode
{
    public partial IdentifierNameSyntax Name { get; }
    public partial TypeAnnotationSyntax TypeAnnotation { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    public VariableDeclaratorSyntax(
        InternalSyntax.VariableDeclaratorSyntax greenNode,
        SyntaxNode parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public VariableDeclaratorSyntax(IdentifierNameSyntax name)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax((InternalSyntax.IdentifierNameSyntax)name.Green), null)
    {

    }

    public VariableDeclaratorSyntax(IdentifierNameSyntax name, TypeAnnotationSyntax typeAnnotation)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.TypeAnnotationSyntax?)typeAnnotation.Green), null)
    {

    }

    public VariableDeclaratorSyntax(IdentifierNameSyntax name, EqualsValueClauseSyntax initializer)
      : this(
            new InternalSyntax.VariableDeclaratorSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.EqualsValueClauseSyntax)initializer.Green), null)
    {

    }

    public VariableDeclaratorSyntax(IdentifierNameSyntax name, TypeAnnotationSyntax typeAnnotation, EqualsValueClauseSyntax initializer)
          : this(
                new InternalSyntax.VariableDeclaratorSyntax((InternalSyntax.IdentifierNameSyntax)name.Green, (InternalSyntax.TypeAnnotationSyntax?)typeAnnotation.Green, (InternalSyntax.EqualsValueClauseSyntax)initializer.Green), null)
    {

    }

    // Additional properties or methods specific to BlockSyntax can be added here.
}

public static partial class SyntaxFactory
{
    public static VariableDeclaratorSyntax VariableDeclarator(IdentifierNameSyntax name)
        => new VariableDeclaratorSyntax(name);

    public static VariableDeclaratorSyntax VariableDeclarator(IdentifierNameSyntax name, TypeAnnotationSyntax typeAnnotationSyntax)
        => new VariableDeclaratorSyntax(name, typeAnnotationSyntax);

    public static VariableDeclaratorSyntax VariableDeclarator(IdentifierNameSyntax name, EqualsValueClauseSyntax initalizer)
        => new VariableDeclaratorSyntax(name, initalizer);

    public static VariableDeclaratorSyntax VariableDeclarator(IdentifierNameSyntax name, TypeAnnotationSyntax typeAnnotationSyntax, EqualsValueClauseSyntax initalizer)
        => new VariableDeclaratorSyntax(name, typeAnnotationSyntax, initalizer);
}