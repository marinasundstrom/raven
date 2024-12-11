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

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitFileScopedNamespaceDeclaration(this);
    }
    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitFileScopedNamespaceDeclaration(this);
    }
}

public static partial class SyntaxFactory
{
    public static FileScopedNamespaceDeclarationSyntax FileScopedNamespaceDeclaration(IdentifierNameSyntax name, SyntaxList<MemberDeclarationSyntax> members)
        => new FileScopedNamespaceDeclarationSyntax(name, SyntaxList<ImportDirectiveSyntax>.Empty, members);
}