namespace Raven.CodeAnalysis.Syntax;

public partial class EnumDeclarationSyntax : BaseTypeDeclarationSyntax
{
    public partial SyntaxToken EnumKeyword { get; }
    public override partial SyntaxToken Identifier { get; }
    public override partial SyntaxToken OpenBraceToken { get; }
    public partial SeparatedSyntaxList<EnumMemberDeclarationSyntax> Members { get; }
    public override partial SyntaxToken CloseBraceToken { get; }
    public override partial SyntaxToken? SemicolonToken { get; }


    internal EnumDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public EnumDeclarationSyntax(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? semicolonToken)
        : this(new InternalSyntax.EnumDeclarationSyntax(enumKeyword.Green, identifier.Green, openBraceToken.Green, members.Green, closeBraceToken.Green, semicolonToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static EnumDeclarationSyntax EnumDeclaration(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? semicolonToken = null)
        => new EnumDeclarationSyntax(enumKeyword, identifier, openBraceToken, members, closeBraceToken, semicolonToken);
}
