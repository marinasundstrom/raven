
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ImportDirectiveSyntax : TypeSyntax
{
    public ImportDirectiveSyntax(SyntaxToken importKeyword, IdentifierNameSyntax @namespace, SyntaxToken semicolonToken)
        : base(SyntaxKind.ImportDirective, [importKeyword, @namespace, semicolonToken])
    {
    }
}