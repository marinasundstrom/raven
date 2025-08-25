
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

        NameSyntax nameSyntax;
        if (PeekToken().Kind == SyntaxKind.SemicolonToken)
        {
            var missing = MissingToken(SyntaxKind.IdentifierToken);
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.IdentifierExpected,
                    GetEndOfLastToken()));
            nameSyntax = IdentifierName(missing);
        }
        else
        {
            nameSyntax = new NameSyntaxParser(this).ParseName();
        }

        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        SetTreatNewlinesAsTokens(false);

        return ImportDirective(importKeyword, nameSyntax, terminatorToken);
    }
}
