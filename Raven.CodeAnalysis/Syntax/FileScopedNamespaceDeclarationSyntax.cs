namespace Raven.CodeAnalysis.Syntax;

public partial class FileScopedNamespaceDeclarationSyntax : BaseNamespaceDeclarationSyntax
{
    public partial SyntaxToken NamespaceKeyword { get; }

    public partial IdentifierNameSyntax Name { get; }

    public partial SyntaxToken SemicolonToken { get; }

    public partial SyntaxList<ImportDirectiveSyntax> Imports { get; }

    public partial SyntaxList<MemberDeclarationSyntax> Members { get; }

    public FileScopedNamespaceDeclarationSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(IdentifierNameSyntax name, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(SyntaxFactory.NamespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, SyntaxFactory.SemicolonToken.Green, imports?.Green, members?.Green), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax(SyntaxToken namespaceKeyword, IdentifierNameSyntax name, SyntaxToken semicolonToken, SyntaxList<ImportDirectiveSyntax> imports, SyntaxList<MemberDeclarationSyntax> members)
        : this(new Syntax.InternalSyntax.FileScopedNamespaceDeclarationSyntax(namespaceKeyword.Green, (InternalSyntax.IdentifierNameSyntax)name.Green, semicolonToken.Green, imports?.Green, members?.Green), (SyntaxNode)null)
    {
    }

    public FileScopedNamespaceDeclarationSyntax WithImports(SyntaxList<ImportDirectiveSyntax> imports)
    {
        return new FileScopedNamespaceDeclarationSyntax(NamespaceKeyword, Name, SemicolonToken, imports, Members);
    }

    public FileScopedNamespaceDeclarationSyntax WithMembers(SyntaxList<MemberDeclarationSyntax> members)
    {
        return new FileScopedNamespaceDeclarationSyntax(NamespaceKeyword, Name, SemicolonToken, Imports, members);
    }
}

public static partial class SyntaxFactory
{
    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(IdentifierNameSyntax name, SyntaxList<MemberDeclarationSyntax> members)
        => new FileScopedNamespaceDeclarationSyntax(name, SyntaxList<ImportDirectiveSyntax>.Empty, members);
}