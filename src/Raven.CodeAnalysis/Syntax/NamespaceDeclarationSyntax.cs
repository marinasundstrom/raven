namespace Raven.CodeAnalysis.Syntax;

public partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken NamespaceKeyword { get; }

    public override partial NameSyntax Name { get; }

    public partial SyntaxToken OpenBraceToken { get; }

    public override partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken CloseBraceToken { get; }

    public partial SyntaxToken? TerminatorToken { get; }

    internal NamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name,
        SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken = null)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(modifiers.Green, namespaceKeyword.Green, (InternalSyntax.NameSyntax)name.Green, openBraceToken.Green, imports.Green, members.Green, closeBraceToken.Green, terminatorToken?.Green), (SyntaxNode)null)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxTokenList modifiers, NameSyntax name,
        SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(modifiers.Green, SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.NameSyntax)name.Green, SyntaxFactory.OpenBraceToken.Green, imports.Green, members.Green, SyntaxFactory.CloseBraceToken.Green, null), (SyntaxNode)null)
    {
    }
}

public static partial class SyntaxFactory
{
    public static NamespaceDeclarationSyntax NamespaceDeclaration(SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? terminatorToken)
        => new NamespaceDeclarationSyntax(SyntaxTokenList.Empty, namespaceKeyword, name, openBraceToken, importDirectives, members, closeBraceToken, terminatorToken);

    public static NamespaceDeclarationSyntax NamespaceDeclaration(SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken)
        => new NamespaceDeclarationSyntax(SyntaxTokenList.Empty, namespaceKeyword, name, openBraceToken, importDirectives, members, closeBraceToken, null);

    public static NamespaceDeclarationSyntax NamespaceDeclaration(NameSyntax name, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
        => new NamespaceDeclarationSyntax(SyntaxTokenList.Empty, name, importDirectives, members);
}