namespace Raven.CodeAnalysis.Syntax;

public partial class PropertyDeclarationSyntax : BasePropertyDeclarationSyntax
{
    internal PropertyDeclarationSyntax(GreenNode greenNode, SyntaxNode? parent, int position) : base(greenNode, parent, position)
    {
    }

    public PropertyDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.PropertyDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.TypeAnnotationClauseSyntax)type.Green, (InternalSyntax.AccessorListSyntax)accessorList.Green, (InternalSyntax.EqualsValueClauseSyntax?)initializer?.Green, terminatorToken?.Green), null, 0)
    {

    }

    public override partial SyntaxTokenList Modifiers { get; }

    public override partial SyntaxToken Identifier { get; }

    public override partial TypeAnnotationClauseSyntax Type { get; }

    public override partial AccessorListSyntax? AccessorList { get; }

    public partial EqualsValueClauseSyntax? Initializer { get; }

    public partial SyntaxToken? TerminatorToken { get; }
}

public static partial class SyntaxFactory
{
    public static PropertyDeclarationSyntax PropertyDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, TypeAnnotationClauseSyntax type, AccessorListSyntax? accessorList, EqualsValueClauseSyntax? initializer, SyntaxToken? terminatorToken)
        => new(modifiers, identifier, type, accessorList, initializer, terminatorToken);
}