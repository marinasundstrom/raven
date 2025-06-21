namespace Raven.CodeAnalysis.Syntax;

public partial class EnumDeclarationSyntax : BaseTypeDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }
    public partial SyntaxToken EnumKeyword { get; }
    public override partial SyntaxToken Identifier { get; }
    public override partial SyntaxToken OpenBraceToken { get; }
    public partial SeparatedSyntaxList<EnumMemberDeclarationSyntax> Members { get; }
    public override partial SyntaxToken CloseBraceToken { get; }
    public override partial SyntaxToken? TerminatorToken { get; }

    internal EnumDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public EnumDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.EnumDeclarationSyntax(modifiers.Green, enumKeyword.Green, identifier.Green, openBraceToken.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static EnumDeclarationSyntax EnumDeclaration(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<EnumMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
        => new EnumDeclarationSyntax(SyntaxTokenList.Empty, enumKeyword, identifier, openBraceToken, members, closeBraceToken, terminatorToken);
}