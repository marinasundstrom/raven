
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ImportDirectiveSyntax : TypeSyntax
{
    public ImportDirectiveSyntax(SyntaxToken importKeyword, IdentifierNameSyntax @namespace)
        : base(SyntaxKind.ImportDirective, [importKeyword, @namespace], importKeyword.FullWidth + @namespace.FullWidth, [], 0)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ImportDirectiveSyntax(this, parent);
    }
}