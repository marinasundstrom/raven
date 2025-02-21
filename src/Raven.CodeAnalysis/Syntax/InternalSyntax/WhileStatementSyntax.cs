
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class WhileStatementSyntax : StatementSyntax
{
    public WhileStatementSyntax(
        SyntaxToken whileKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        SyntaxToken semicolonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.WhileStatement,
              [
                whileKeyword ?? throw new ArgumentNullException(nameof(whileKeyword)),
                openParenToken ?? throw new ArgumentNullException(nameof(openParenToken)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                closeParenToken ?? throw new ArgumentNullException(nameof(closeParenToken)),
                statement ?? throw new ArgumentNullException(nameof(statement)),
                semicolonToken ?? throw new ArgumentNullException(nameof(semicolonToken))
              ],
              diagnostics)
    {
    }

    public WhileStatementSyntax(
        SyntaxToken whileKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : base(
          SyntaxKind.WhileStatement,
          [
                whileKeyword,
                openParenToken,
                condition,
                closeParenToken,
                statement,
          ],
          diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static WhileStatementSyntax WhileStatement(
        SyntaxToken whileKeyword,
        SyntaxToken openParenToken,
        SyntaxNode condition,
        SyntaxToken closeParenToken,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(whileKeyword, openParenToken, condition, closeParenToken, statement, diagnostics);
}