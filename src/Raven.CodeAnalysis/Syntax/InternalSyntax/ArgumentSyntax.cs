namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArgumentSyntax : SyntaxNode
{
    public ArgumentSyntax(
        ExpressionSyntax expression,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.Argument, [
            expression
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ArgumentSyntax Argument(
        ExpressionSyntax expression,
        IEnumerable<Diagnostic>? diagnostics = null)
        => new(expression, diagnostics);
}