
namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class ImportDirectiveSyntaxParser : SyntaxParser
{
    public ImportDirectiveSyntaxParser(ParseContext parent) : base(parent)
    {

    }

    public ImportDirectiveSyntax ParseImportDirective()
    {
        var importKeyword = ReadToken();

        var namespaceName = new NameSyntaxParser(this).ParseName();

        if (!ConsumeTokenOrMissing(SyntaxKind.SemicolonToken, out var semicolonToken))
        {
            return ImportDirective(importKeyword, namespaceName, semicolonToken,
                [DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()
                )]);
        }

        return ImportDirective(importKeyword, namespaceName, semicolonToken);
    }
}