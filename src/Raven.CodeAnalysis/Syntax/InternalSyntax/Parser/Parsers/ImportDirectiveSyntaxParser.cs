
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

        NameEqualsSyntax? alias = null;
        if (PeekToken().Kind == SyntaxKind.IdentifierToken && PeekToken(1).Kind == SyntaxKind.EqualsToken)
        {
            var name = IdentifierName(ReadToken());
            var equals = ReadToken();
            alias = NameEquals(name, equals);
        }

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

        if (!TryConsumeTerminator(out var terminatorToken))
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.SemicolonExpected,
                    GetEndOfLastToken()));
        }

        SetTreatNewlinesAsTokens(false);

        return ImportDirective(importKeyword, alias, nameSyntax, terminatorToken);
    }
}
