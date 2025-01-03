namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal abstract class StatementSyntax : SyntaxNode
{
    public StatementSyntax(SyntaxKind kind, GreenNode[] slots,
        IEnumerable<Diagnostic>? diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}