namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class MemberAccessExpressionSyntax : ExpressionSyntax
{
    public MemberAccessExpressionSyntax(
        SyntaxKind kind,
        ExpressionSyntax expression,
        SyntaxToken operatorToken,
        SimpleNameSyntax name,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(
              kind,
              [
                      expression,
                      operatorToken,
                      name
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
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(kind, expression, operatorToken, name, diagnostics);
}