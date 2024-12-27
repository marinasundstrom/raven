namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class EmptyStatementSyntax : StatementSyntax
{
    public EmptyStatementSyntax(SyntaxToken semicolonToken)
        : base(
              SyntaxKind.EmptyStatement,
              [
                      semicolonToken
              ])
    {
    }
}