namespace Raven.CodeAnalysis.Syntax;

public partial class ImportDirectiveSyntax : SyntaxNode
{
    public partial SyntaxToken ImportKeyword { get; }

    public partial IdentifierNameSyntax Namespace { get; }

    internal ImportDirectiveSyntax(GreenNode greenNode, SyntaxNode parent, int position = 0)
        : base(greenNode, parent, position)
    {
    }

    public ImportDirectiveSyntax(SyntaxToken importKeyword, IdentifierNameSyntax @namespace)
        : base(new InternalSyntax.ImportDirectiveSyntax(importKeyword.Green, (InternalSyntax.IdentifierNameSyntax)@namespace.Green), (SyntaxNode)null)
    {

    }

    public override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitImportDirective(this);
    }

    public override TNode Accept<TNode>(SyntaxVisitor<TNode> visitor)
    {
        return visitor.VisitImportDirective(this);
    }
}

public static partial class SyntaxFactory
{
    public static ImportDirectiveSyntax ImportDirective(IdentifierNameSyntax @namespace)
        => new ImportDirectiveSyntax(SyntaxFactory.ImportKeyword, @namespace);

    public static ImportDirectiveSyntax ImportDirective(SyntaxToken importKeyword, IdentifierNameSyntax @namespace)
        => new ImportDirectiveSyntax(importKeyword, @namespace);
}