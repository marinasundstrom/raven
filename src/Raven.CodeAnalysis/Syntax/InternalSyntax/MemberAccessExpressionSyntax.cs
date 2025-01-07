namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MemberAccessExpressionSyntax : ExpressionSyntax
{
    public MemberAccessExpressionSyntax(
        SyntaxKind kind,
        ExpressionSyntax expression,
        SyntaxToken operatorToken,
        SimpleNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              kind,
              [
                      expression ?? throw new ArgumentNullException(nameof(expression)),
                      operatorToken ?? throw new ArgumentNullException(nameof(operatorToken)),
                      name ?? throw new ArgumentNullException(nameof(name))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static MemberAccessExpressionSyntax MemberAccessExpression(
        SyntaxKind kind,
        ExpressionSyntax expression,
        SyntaxToken operatorToken,
        SimpleNameSyntax name,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, expression, operatorToken, name, diagnostics);
}