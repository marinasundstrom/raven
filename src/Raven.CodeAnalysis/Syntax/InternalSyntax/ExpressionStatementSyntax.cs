namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(ExpressionSyntax expression, SyntaxToken semicolonToken)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression,
                    semicolonToken,
              ])
    {
    }
}