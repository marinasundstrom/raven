namespace Raven.CodeAnalysis.Syntax;

public partial class ImportDirectiveSyntax : SyntaxNode
{
    public partial SyntaxToken ImportKeyword { get; }

    public partial IdentifierNameSyntax Namespace { get; }

    public partial SyntaxToken SemicolonToken { get; }

    internal ImportDirectiveSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ImportDirectiveSyntax(SyntaxToken importKeyword, IdentifierNameSyntax @namespace, SyntaxToken semicolonToken)
        : base(new InternalSyntax.ImportDirectiveSyntax(importKeyword.Green, (InternalSyntax.IdentifierNameSyntax)@namespace.Green, semicolonToken.Green), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(IdentifierNameSyntax @namespace)
        => new ImportDirectiveSyntax(SyntaxFactory.ImportKeyword, @namespace, SyntaxFactory.SemicolonToken);

    public static ImportDirectiveSyntax ImportDirective(SyntaxToken importKeyword, IdentifierNameSyntax @namespace, SyntaxToken semicolonToken)
        => new ImportDirectiveSyntax(importKeyword, @namespace, semicolonToken);
}