namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ExpressionStatementSyntax : StatementSyntax
{
    public ExpressionStatementSyntax(ExpressionSyntax expression)
        : base(SyntaxKind.ExpressionStatement,
              [
                    expression
              ])
    {
    }
}