namespace Raven.CodeAnalysis.Syntax;

public partial class ClassDeclarationSyntax : TypeDeclarationSyntax
{
    public override int Arity { get; }

    public override partial SyntaxTokenList Modifiers { get; }

    public override partial SyntaxToken Keyword { get; }

    public override partial SyntaxToken Identifier { get; }

    public override partial ParameterListSyntax? ParameterList { get; }

    public override partial SyntaxToken OpenBraceToken { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public override partial SyntaxToken CloseBraceToken { get; }

    public override partial SyntaxToken? TerminatorToken { get; }

    internal ClassDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ClassDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken keyword, SyntaxToken identifier, ParameterListSyntax parameterList, SyntaxToken openBraceToken, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.ClassDeclarationSyntax(modifiers.Green, keyword.Green, identifier.Green, (InternalSyntax.ParameterListSyntax)parameterList.Green, openBraceToken.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ClassDeclarationSyntax ClassDeclaration(SyntaxToken keyword, SyntaxToken identifier, SyntaxToken openBraceToken, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
    => new ClassDeclarationSyntax(SyntaxTokenList.Empty, keyword, identifier, null, openBraceToken, members, closeBraceToken, terminatorToken);

    public static ClassDeclarationSyntax ClassDeclaration(SyntaxTokenList modifiers, SyntaxToken keyword, SyntaxToken identifier, SyntaxToken openBraceToken, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
        => new ClassDeclarationSyntax(modifiers, keyword, identifier, null, openBraceToken, members, closeBraceToken, terminatorToken);
}