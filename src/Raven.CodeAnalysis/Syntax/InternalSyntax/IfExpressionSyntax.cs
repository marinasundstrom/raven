
namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class IfExpressionSyntax : ExpressionSyntax
{
    public IfExpressionSyntax(
        SyntaxToken ifKeyword,
        SyntaxNode condition,
        ExpressionSyntax expression,
        ElseClauseSyntax elseClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.IfExpression,
              [
                ifKeyword ?? throw new ArgumentNullException(nameof(ifKeyword)),
                condition ?? throw new ArgumentNullException(nameof(condition)),
                expression ?? throw new ArgumentNullException(nameof(expression)),
                elseClause
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static IfExpressionSyntax IfExpression(
        SyntaxToken ifKeyword,
        SyntaxNode condition,
        ExpressionSyntax expression,
        ElseClauseSyntax elseClause,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(ifKeyword, condition, expression, elseClause, diagnostics);
}