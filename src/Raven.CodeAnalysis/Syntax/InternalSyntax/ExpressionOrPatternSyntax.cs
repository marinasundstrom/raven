namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class ExpressionOrPatternSyntax : SyntaxNode
{
    public ExpressionOrPatternSyntax(
        SyntaxKind kind,
        GreenNode[] slots,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, slots, diagnostics)
    {
    }
}