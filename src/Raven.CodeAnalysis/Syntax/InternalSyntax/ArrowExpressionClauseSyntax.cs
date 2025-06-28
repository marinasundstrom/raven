namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArrowExpressionClauseSyntax : SyntaxNode
{
    public ArrowExpressionClauseSyntax(
        SyntaxToken arrowToken,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.ArrowExpressionClause, [
            arrowToken ?? throw new ArgumentNullException(nameof(arrowToken)),
            expression ?? throw new ArgumentNullException(nameof(expression))
    ],
    diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ArrowExpressionClauseSyntax ArrowExpressionClause(
        SyntaxToken arrowToken,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(arrowToken, expression, diagnostics);
}