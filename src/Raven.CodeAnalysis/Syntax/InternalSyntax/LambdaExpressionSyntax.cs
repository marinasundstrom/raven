namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract partial class LambdaExpressionSyntax : ExpressionSyntax
{
    protected LambdaExpressionSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}