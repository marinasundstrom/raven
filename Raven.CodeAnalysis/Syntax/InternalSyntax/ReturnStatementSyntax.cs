namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ReturnStatementSyntax : StatementSyntax
{
    public ReturnStatementSyntax(
        SyntaxToken returnKeyword,
        ExpressionSyntax expression,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.ReturnStatement,
              [
                      returnKeyword,
                      expression,
                      semicolonToken
              ],
              diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.ReturnStatementSyntax(this, parent, position);
    }
}