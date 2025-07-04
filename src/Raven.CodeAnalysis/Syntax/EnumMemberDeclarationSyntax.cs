namespace Raven.CodeAnalysis.Syntax;

public partial class EnumMemberDeclarationSyntax : MemberDeclarationSyntax
{
    internal EnumMemberDeclarationSyntax(
        GreenNode greenNode,
        SyntaxNode? parent = null,
        int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public EnumMemberDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken identifier, EqualsValueClauseSyntax? equalsValueClauseSyntax)
        : this(new InternalSyntax.EnumMemberDeclarationSyntax(modifiers.Green, identifier.Green, (InternalSyntax.EqualsValueClauseSyntax?)equalsValueClauseSyntax?.Green, null))
    {
    }

    public override partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken Identifier { get; }
    public partial EqualsValueClauseSyntax? EqualsValueClauseSyntax { get; }
}

public static partial class SyntaxFactory
{
    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier)
        => new EnumMemberDeclarationSyntax(modifiers, identifier, null);

    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(SyntaxTokenList modifiers, SyntaxToken identifier, EqualsValueClauseSyntax? equalsValueClauseSyntax)
        => new EnumMemberDeclarationSyntax(modifiers, identifier, equalsValueClauseSyntax);
}