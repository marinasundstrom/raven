namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class SelfExpressionSyntax : ExpressionSyntax
{
    public SelfExpressionSyntax(
        SyntaxToken selfKeyword,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.SelfExpression,
              [
                      selfKeyword ?? throw new ArgumentNullException(nameof(selfKeyword))
              ],
              diagnostics)
    { }
}

internal static partial class SyntaxFactory
{
    public static SelfExpressionSyntax SelfExpression(
        SyntaxToken selfKeyword,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(selfKeyword, diagnostics);
}