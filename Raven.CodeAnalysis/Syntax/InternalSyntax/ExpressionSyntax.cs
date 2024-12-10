namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public abstract class ExpressionSyntax : SyntaxNode
{
    public ExpressionSyntax(SyntaxKind kind, GreenNode[] slots, IEnumerable<DiagnosticInfo> diagnostics = null) : base(kind, slots, diagnostics)
    {
    }
}