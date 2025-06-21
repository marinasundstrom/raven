namespace Raven.CodeAnalysis.Syntax;

public partial class ClassDeclarationSyntax : TypeDeclarationSyntax
{
    public override partial int Arity { get; }

    public override partial SyntaxToken Keyword { get; }

    public override partial SyntaxToken Identifier { get; }

    public override partial ParameterListSyntax ParameterList { get; }

    public override partial SyntaxToken OpenBraceToken { get; }

    public override partial SyntaxList<MemberDeclarationSyntax>? Members { get; }

    public override partial SyntaxToken CloseBraceToken { get; }

    public override partial SyntaxToken? TerminatorToken { get; }

    internal ClassDeclarationSyntax(InternalSyntax.SyntaxNode greenNode, SyntaxNode? parent = null, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ClassDeclarationSyntax(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<ClassMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken)
        : this(new InternalSyntax.ClassDeclarationSyntax(enumKeyword.Green, identifier.Green, openBraceToken.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green, null))
    {
    }
}

public static partial class SyntaxFactory
{
    public static ClassDeclarationSyntax ClassDeclaration(SyntaxToken enumKeyword, SyntaxToken identifier, SyntaxToken openBraceToken, SeparatedSyntaxList<ClassMemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
        => new ClassDeclarationSyntax(enumKeyword, identifier, openBraceToken, members, closeBraceToken, terminatorToken);
}