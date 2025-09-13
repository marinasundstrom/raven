namespace Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

internal class EqualsValueClauseSyntaxParser : SyntaxParser
{
    public EqualsValueClauseSyntaxParser(ParseContext context) : base(context)
    {

    }

    public EqualsValueClauseSyntax? Parse()
    {
        var equalsToken = ReadToken();

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
