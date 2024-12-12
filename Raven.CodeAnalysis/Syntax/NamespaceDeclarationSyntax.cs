namespace Raven.CodeAnalysis.Syntax;

public partial class NamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public partial SyntaxToken NamespaceKeyword { get; }

    public partial IdentifierNameSyntax Name { get; }

    public partial SyntaxToken OpenBraceToken { get; }

    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public partial SyntaxToken CloseBraceToken { get; }

    public partial SyntaxToken? SemicolonToken { get; }

    public NamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public NamespaceDeclarationSyntax(SyntaxToken namespaceKeyword, IdentifierNameSyntax name,
        SyntaxToken openBraceToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members, SyntaxToken closeBraceToken, SyntaxToken? semicolonToken = null)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, openBraceToken.Green, imports?.Green, members?.Green, closeBraceToken.Green, SyntaxFactory.SemicolonToken.Green), (SyntaxNode)null)
    {
    }

    public NamespaceDeclarationSyntax(IdentifierNameSyntax name,
        SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.NamespaceDeclarationSyntax(SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, SyntaxFactory.OpenBraceToken.Green, imports?.Green, members?.Green, SyntaxFactory.CloseBraceToken.Green, SyntaxFactory.SemicolonToken.Green), (SyntaxNode)null)
    {
    }
}

public static partial class SyntaxFactory
{
    public static NamespaceDeclarationSyntax NamespaceDeclaration(IdentifierNameSyntax name, SyntaxList<MemberDeclarationSyntax> members)
        => new NamespaceDeclarationSyntax(name, SyntaxList<ImportDirectiveSyntax>.Empty, members);
}