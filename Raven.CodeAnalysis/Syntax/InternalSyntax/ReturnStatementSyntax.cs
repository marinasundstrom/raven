namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ReturnStatementSyntax : StatementSyntax
{
    public ReturnStatementSyntax(
        SyntaxToken returnKeyword,
        ExpressionSyntax expression,
        SyntaxToken semicolonToken)
        : base(
              SyntaxKind.ReturnStatement,
              [
                      returnKeyword,
                      expression,
                      semicolonToken
              ])
    {
    }
}