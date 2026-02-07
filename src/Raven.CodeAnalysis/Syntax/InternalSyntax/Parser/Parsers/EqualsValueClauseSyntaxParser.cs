namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class EqualsValueClauseSyntaxParser : SyntaxParser
{
    public EqualsValueClauseSyntaxParser(ParseContext context) : base(context)
    {

    }

    public EqualsValueClauseSyntax? Parse()
    {
        var equalsToken = ReadToken();

        // Ensure that newlines terminate the initializer so subsequent statements
        // (such as tuple assignments) aren't consumed as part of the expression.
        var previous = TreatNewlinesAsTokens;
        SetTreatNewlinesAsTokens(false);

        var expr = new ExpressionSyntaxParser(this).ParseExpression();

        SetTreatNewlinesAsTokens(previous);

        if (expr.IsMissing)
        {
            AddDiagnostic(
                DiagnosticInfo.Create(
                    CompilerDiagnostics.ExpressionExpected,
                    GetEndOfLastToken()));
        }

        return SyntaxFactory.EqualsValueClause(equalsToken, expr);
    }
}
