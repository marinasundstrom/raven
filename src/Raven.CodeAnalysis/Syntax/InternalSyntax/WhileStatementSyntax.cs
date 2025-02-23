
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class WhileStatementSyntax : StatementSyntax
{
    public WhileStatementSyntax(
        SyntaxToken whileKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.WhileStatement,
              [
                whileKeyword ?? throw new ArgumentNullException(nameof(whileKeyword)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                statement ?? throw new ArgumentNullException(nameof(statement)),
                semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }

    public WhileStatementSyntax(
        SyntaxToken whileKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
          SyntaxKind.WhileStatement,
          [
            whileKeyword ?? throw new ArgumentNullException(nameof(whileKeyword)),
            condition ?? throw new ArgumentNullException(nameof(condition)),
            statement ?? throw new ArgumentNullException(nameof(statement)),
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static WhileStatementSyntax WhileStatement(
        SyntaxToken whileKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(whileKeyword, condition, statement, diagnostics);
}