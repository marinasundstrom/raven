namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using static Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory;

internal class AliasDirectiveSyntaxParser : SyntaxParser
{
    public AliasDirectiveSyntaxParser(ParseContext parent) : base(parent)
    {
    }

    public AliasDirectiveSyntax ParseAliasDirective()
    {
        var aliasKeyword = ReadToken();
        var identifier = ReadToken();
        var equalsToken = ReadToken();

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

        return AliasDirective(aliasKeyword, identifier, equalsToken, nameSyntax, terminatorToken);
    }
}
