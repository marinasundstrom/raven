namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(ExpressionSyntax expression)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression
              ])
    {
    }
}