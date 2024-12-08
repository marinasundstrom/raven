namespace Raven.CodeAnalysis.Syntax;

public partial class ImportDirectiveSyntax : SyntaxNode
{
    public partial SyntaxToken ImportKeyword { get; }

    public partial IdentifierNameSyntax Namespace { get; }

    internal ImportDirectiveSyntax(GreenNode greenNode, SyntaxNode parent)
    : base(greenNode, parent)
    {
    }

    public ImportDirectiveSyntax(SyntaxToken importKeyword, IdentifierNameSyntax @namespace)
    : base(new InternalSyntax.ImportDirectiveSyntax(importKeyword.Green, (InternalSyntax.IdentifierNameSyntax)@namespace.Green), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(IdentifierNameSyntax @namespace)
        => new ImportDirectiveSyntax(SyntaxFactory.ImportKeyword, @namespace);

    public static ImportDirectiveSyntax ImportDirective(SyntaxToken importKeyword, IdentifierNameSyntax @namespace)
        => new ImportDirectiveSyntax(importKeyword, @namespace);
}
