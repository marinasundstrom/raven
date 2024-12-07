namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class ReturnStatementSyntax : StatementSyntax
{
    public ReturnStatementSyntax(
        SyntaxToken returnKeyword,
        ExpressionSyntax expression,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.ReturnStatement,
              [
                      returnKeyword,
                      expression
              ],
              returnKeyword.FullWidth + expression.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.ReturnStatementSyntax(this, parent);
    }
}
