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

    public EnumMemberDeclarationSyntax(SyntaxToken identifier, EqualsValueClauseSyntax? equalsValueClauseSyntax)
        : this(new InternalSyntax.EnumMemberDeclarationSyntax(identifier.Green, (InternalSyntax.EqualsValueClauseSyntax?)equalsValueClauseSyntax?.Green, null))
    {
    }

    public partial SyntaxToken Identifier { get; }
    public partial EqualsValueClauseSyntax? EqualsValueClauseSyntax { get; }
}

public static partial class SyntaxFactory
{
    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(SyntaxToken identifier)
        => new EnumMemberDeclarationSyntax(identifier, null);

    public static EnumMemberDeclarationSyntax EnumMemberDeclaration(SyntaxToken identifier, EqualsValueClauseSyntax? equalsValueClauseSyntax)
        => new EnumMemberDeclarationSyntax(identifier, equalsValueClauseSyntax);
}
