namespace Raven.CodeAnalysis.Syntax;

public partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public override partial SyntaxTokenList Modifiers { get; }

    public partial SyntaxToken NamespaceKeyword { get; }

    public override partial NameSyntax Name { get; }

    public partial SyntaxToken TerminatorToken { get; }

    public override partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public override partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    internal FileScopedNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxTokenList modifiers, NameSyntax name, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(modifiers.Green, SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, SyntaxFactory.SemicolonToken.Green, imports.Green, members.Green), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxTokenList modifiers,SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken)
    : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(modifiers.Green, namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, terminatorToken.Green, InternalSyntax.SyntaxList.Empty, InternalSyntax.SyntaxList.Empty), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(modifiers.Green, namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, terminatorToken.Green, imports.Green, members.Green), (SyntaxNode)null)
    {
    }
}

public static partial class SyntaxFactory
{
    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken)
    => new FileScopedNamespaceDeclarationSyntax(modifiers, namespaceKeyword, name, terminatorToken);

    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(SyntaxTokenList modifiers, SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken terminatorToken, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
    => new FileScopedNamespaceDeclarationSyntax(modifiers, namespaceKeyword, name, terminatorToken, importDirectives, members);

    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(SyntaxTokenList modifiers, NameSyntax name, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
        => new FileScopedNamespaceDeclarationSyntax(modifiers, name, importDirectives, members);
}