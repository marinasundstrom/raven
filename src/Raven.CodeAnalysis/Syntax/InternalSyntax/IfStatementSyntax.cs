
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class IfStatementSyntax : StatementSyntax
{
    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.IfStatement,
              [
                ifKeyword ?? throw new ArgumentNullException(nameof(ifKeyword)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                statement ?? throw new ArgumentNullException(nameof(statement)),
                elseClause ?? throw new ArgumentNullException(nameof(elseClause)),
                semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }

    public IfStatementSyntax(
        SyntaxToken ifKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
          SyntaxKind.IfStatement,
          [
             ifKeyword ?? throw new ArgumentNullException(nameof(ifKeyword)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                statement ?? throw new ArgumentNullException(nameof(statement)),
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
        SyntaxNode condition,
        StatementSyntax statement,
        ElseClauseSyntax elseClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(ifKeyword, condition, statement, elseClause, diagnostics);
}