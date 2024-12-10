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
              diagnostics)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        ExpressionSyntax condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
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
          diagnostics)
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
              diagnostics)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
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
          diagnostics)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent, int position)
    {
        return new Syntax.IfStatementSyntax(this, parent, position);
    }
}