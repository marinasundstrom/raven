namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class StatementSyntax : SyntaxNode
{
    public StatementSyntax(SyntaxKind kind, GreenNode[] slots, int fullWidth, IEnumerable<DiagnosticInfo> diagnostics = null, int startPosition = 0) : base(kind, slots, fullWidth, diagnostics, startPosition)
    {
    }
}
