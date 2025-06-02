namespace Raven.CodeAnalysis.Syntax;

public partial class ImportDirectiveSyntax : SyntaxNode
{
    public partial SyntaxToken ImportKeyword { get; }

    public partial TypeSyntax NamespaceOrType { get; }

    public partial SyntaxToken TerminationToken { get; }

    internal ImportDirectiveSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ImportDirectiveSyntax(SyntaxToken importKeyword, TypeSyntax namespaceOrType, SyntaxToken terminationToken)
        : base(new InternalSyntax.ImportDirectiveSyntax(importKeyword.Green, (InternalSyntax.NameSyntax)namespaceOrType.Green, terminationToken.Green), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(NameSyntax @namespace)
        => new ImportDirectiveSyntax(ImportKeyword, @namespace, SemicolonToken);

    public static ImportDirectiveSyntax ImportDirective(SyntaxToken importKeyword, TypeSyntax namespaceOrType, SyntaxToken terminationToken)
        => new ImportDirectiveSyntax(importKeyword, namespaceOrType, terminationToken);
}