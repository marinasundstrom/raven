namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class CollectionElementSyntax : SyntaxNode
{
    public CollectionElementSyntax(
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.CollectionElement, [
            expression ?? throw new ArgumentNullException(nameof(expression))
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static CollectionElementSyntax CollectionElement(
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(expression, diagnostics);
}