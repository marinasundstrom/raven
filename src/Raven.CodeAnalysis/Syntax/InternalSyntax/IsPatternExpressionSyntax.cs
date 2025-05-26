namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class IsPatternExpressionSyntax : ExpressionSyntax
{
    public IsPatternExpressionSyntax(
        ExpressionSyntax expression,
        SyntaxToken isKeyword,
        PatternSyntax pattern,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.IsPatternExpression,
              [
                    expression ?? throw new ArgumentNullException(nameof(expression)),
                    isKeyword ?? throw new ArgumentNullException(nameof(isKeyword)),
                    pattern ?? throw new ArgumentNullException(nameof(pattern))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static IsPatternExpressionSyntax IsPatternExpression(
        ExpressionSyntax expression,
        SyntaxToken isKeyword,
        PatternSyntax pattern,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(expression, isKeyword, pattern, diagnostics);
}