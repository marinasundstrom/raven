namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class EqualsValueClauseSyntaxParser : SyntaxParser
{
    public EqualsValueClauseSyntaxParser(ParseContext context) : base(context)
    {

    }

    public EqualsValueClauseSyntax? Parse()
    {
        var equalsToken = ReadToken();

        // Ensure that any newline following the '=' is treated as trivia so the
        // initializer expression can continue on the next line without being
        // prematurely terminated.
        SetTreatNewlinesAsTokens(false);

        var expr = new ExpressionSyntaxParser(this).ParseExpression();

        if (expr.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.ExpressionExpected,
                    GetEndOfLastToken()));
        }

        return SyntaxFactory.EqualsValueClause(equalsToken, expr, Diagnostics);
    }
}
