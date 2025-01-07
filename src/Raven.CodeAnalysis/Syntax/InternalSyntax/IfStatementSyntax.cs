
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class IfStatementSyntax : StatementSyntax
{
    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.IfStatement,
              [
                ifKeyword ?? throw new ArgumentNullException(nameof(ifKeyword)),
                openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken)),
                statement ?? throw new ArgumentNullException(nameof(statement)),
                elseClause ?? throw new ArgumentNullException(nameof(elseClause)),
                semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
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
        IEnumerable<DiagnosticInfo>? diagnostics = null)
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
}

internal static partial class SyntaxFactory
{
    public static IfStatementSyntax IfStatement(
        SyntaxToken ifKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(ifKeyword, openParenToken, condition, closeParenToken, statement, elseClause, diagnostics);
}