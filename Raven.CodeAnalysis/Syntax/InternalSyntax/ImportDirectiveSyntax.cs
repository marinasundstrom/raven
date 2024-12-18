
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ImportDirectiveSyntax : SyntaxNode
{
    public ImportDirectiveSyntax(SyntaxToken importKeyword, NameSyntax @namespace, SyntaxToken semicolonToken)
        : base(SyntaxKind.ImportDirective, [importKeyword, @namespace, semicolonToken])
    {
    }
}