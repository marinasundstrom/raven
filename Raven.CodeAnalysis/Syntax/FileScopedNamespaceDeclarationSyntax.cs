namespace Raven.CodeAnalysis.Syntax;

public partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public partial SyntaxToken NamespaceKeyword { get; }

    public partial NameSyntax Name { get; }

    public partial SyntaxToken SemicolonToken { get; }

    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    internal FileScopedNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(NameSyntax name, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, SyntaxFactory.SemicolonToken.Green, imports.Green, members.Green), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken semicolonToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, semicolonToken.Green, imports.Green, members.Green), (SyntaxNode)null)
    {
    }
}

public static partial class SyntaxFactory
{
    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(SyntaxToken namespaceKeyword, NameSyntax name, SyntaxToken semicolonToken, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
    => new FileScopedNamespaceDeclarationSyntax(namespaceKeyword, name, semicolonToken, importDirectives, members);

    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(NameSyntax name, SyntaxList<ImportDirectiveSyntax> importDirectives, SyntaxList<MemberDeclarationSyntax> members)
        => new FileScopedNamespaceDeclarationSyntax(name, importDirectives, members);
}