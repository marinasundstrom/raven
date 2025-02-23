
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class WhileExpressionSyntax : ExpressionSyntax
{
    public WhileExpressionSyntax(
        SyntaxToken whileKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.WhileExpression,
              [
                whileKeyword ?? throw new ArgumentNullException(nameof(whileKeyword)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                statement ?? throw new ArgumentNullException(nameof(statement))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static WhileExpressionSyntax WhileStatement(
        SyntaxToken whileKeyword,
        SyntaxNode condition,
        StatementSyntax statement,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(whileKeyword, condition, statement, diagnostics);
}