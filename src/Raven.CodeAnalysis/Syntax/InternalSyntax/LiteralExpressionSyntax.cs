namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class LiteralExpressionSyntax : ExpressionSyntax
{
    public LiteralExpressionSyntax(
        SyntaxKind kind,
        SyntaxToken token,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, [token ?? throw new ArgumentNullException(nameof(token))], diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static LiteralExpressionSyntax LiteralExpression(
        SyntaxKind kind,
        SyntaxToken token,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, token, diagnostics);
}