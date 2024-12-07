namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxNode : GreenNode
{
    private readonly GreenNode[] _slots;

    public SyntaxNode(
        SyntaxKind kind,
        GreenNode[] slots,
        int fullWidth,
        IEnumerable<DiagnosticInfo> diagnostics = null,
        int startPosition = 0)
        : base((Syntax.SyntaxKind)kind, slots?.Length ?? 0, fullWidth, diagnostics, startPosition)
    {
        _slots = slots ?? Array.Empty<GreenNode>();
    }

    public override GreenNode GetSlot(int index)
    {
        if (index < 0 || index >= SlotCount)
            return null;
        //throw new IndexOutOfRangeException($"Invalid slot index: {index}");
        return _slots[index];
    }
}