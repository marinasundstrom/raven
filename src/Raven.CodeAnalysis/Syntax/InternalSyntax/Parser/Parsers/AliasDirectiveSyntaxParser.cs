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

        TypeSyntax target;
        if (PeekToken().Kind == SyntaxKind.SemicolonToken)
        {
            var missing = MissingToken(SyntaxKind.IdentifierToken);
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.IdentifierExpected,
                    GetEndOfLastToken()));
            target = IdentifierName(missing);
        }
        else
        {
            target = new NameSyntaxParser(this).ParseTypeName();
        }

        SetTreatNewlinesAsTokens(true);

        TryConsumeTerminator(out var terminatorToken);

        SetTreatNewlinesAsTokens(false);

        return AliasDirective(aliasKeyword, identifier, equalsToken, target, terminatorToken);
    }
}
