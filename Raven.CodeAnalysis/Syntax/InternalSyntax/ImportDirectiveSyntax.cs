
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ImportDirectiveSyntax : TypeSyntax
{
    public ImportDirectiveSyntax(SyntaxToken importKeyword, IdentifierNameSyntax @namespace, SyntaxToken semicolonToken)
        : base(SyntaxKind.ImportDirective, [importKeyword, @namespace, semicolonToken])
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ImportDirectiveSyntax(this, parent, position);
    }
}