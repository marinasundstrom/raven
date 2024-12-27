namespace Raven.CodeAnalysis.Syntax;

public partial class ImportDirectiveSyntax : SyntaxNode
{
    public partial SyntaxToken ImportKeyword { get; }

    public partial NameSyntax Namespace { get; }

    public partial SyntaxToken SemicolonToken { get; }

    internal ImportDirectiveSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ImportDirectiveSyntax(SyntaxToken importKeyword, NameSyntax @namespace, SyntaxToken semicolonToken)
        : base(new InternalSyntax.ImportDirectiveSyntax(importKeyword.Green, (InternalSyntax.NameSyntax)@namespace.Green, semicolonToken.Green), (SyntaxNode)null)
    {

    }
}

public static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(NameSyntax @namespace)
        => new ImportDirectiveSyntax(ImportKeyword, @namespace, SemicolonToken);

    public static ImportDirectiveSyntax ImportDirective(SyntaxToken importKeyword, NameSyntax @namespace, SyntaxToken semicolonToken)
        => new ImportDirectiveSyntax(importKeyword, @namespace, semicolonToken);
}