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

        if (expr is null)
        {
            /*
            DiagnosticBag.Add(
                InternalDiagnostic.Create(
                    CompilerDiagnostics.ExpressionExpected,
                    new Location(
                        new TextSpan(currentSpanPosition, 1))
                ));
            */
        }

        return SyntaxFactory.EqualsValueClause(equalsToken, expr);
    }
}