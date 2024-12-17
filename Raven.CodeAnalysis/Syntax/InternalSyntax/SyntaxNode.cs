namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxNode : GreenNode
{
    private readonly GreenNode[] _slots;

    public SyntaxNode(
        SyntaxKind kind,
        GreenNode[] slots)
        : base(kind, slots?.Length ?? 0, CalculateWidth(slots), CalculateFullWidth(slots))
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

    public override int GetChildStartPosition(int childIndex)
    {
        int offset = 0;

        for (int i = 0; i < childIndex; i++)
        {
            var slot = GetSlot(i);
            if (slot != null)
            {
                offset += slot.FullWidth;
            }
        }

        return offset;
    }

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return new SyntaxNode(Kind, newChildren);
    }
}