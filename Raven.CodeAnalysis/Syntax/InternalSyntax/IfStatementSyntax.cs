namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class IfStatementSyntax : StatementSyntax
{
    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        ExpressionSyntax condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        SyntaxToken semicolonToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.IfStatement,
              [
                      ifKeyword,
                      openParenToken,
                      condition,
                      closeParenToken,
                      statement,
                      semicolonToken
              ],
              ifKeyword.FullWidth + openParenToken.FullWidth + (condition?.FullWidth ?? 0) + closeParenToken.FullWidth + (statement?.FullWidth ?? 0) + semicolonToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        ExpressionSyntax condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
    : base(
          SyntaxKind.IfStatement,
          [
                  ifKeyword,
                      openParenToken,
                      condition,
                      closeParenToken,
                      statement,
          ],
          ifKeyword.FullWidth + openParenToken.FullWidth + (condition?.FullWidth ?? 0) + closeParenToken.FullWidth + (statement?.FullWidth ?? 0),
          diagnostics,
          startPosition)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        SyntaxToken semicolonToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.IfStatement,
              [
                      ifKeyword,
                          openParenToken,
                          condition,
                          closeParenToken,
                          statement,
                          elseClause,
                          semicolonToken
              ],
              ifKeyword.FullWidth + openParenToken.FullWidth + condition.FullWidth + closeParenToken.FullWidth + statement.FullWidth + (elseClause?.FullWidth ?? 0) + semicolonToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        int startPosition = 0,
    IEnumerable<DiagnosticInfo> diagnostics = null)
    : base(
          SyntaxKind.IfStatement,
          [
                  ifKeyword,
                          openParenToken,
                          condition,
                          closeParenToken,
                          statement,
                          elseClause
          ],
          ifKeyword.FullWidth + openParenToken.FullWidth + condition.FullWidth + closeParenToken.FullWidth + statement.FullWidth + (elseClause?.FullWidth ?? 0),
          diagnostics,
          startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.IfStatementSyntax(this, parent);
    }
}
