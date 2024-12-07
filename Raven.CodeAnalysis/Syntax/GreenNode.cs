namespace Raven.CodeAnalysis.Syntax;
public abstract class GreenNode
{
    public SyntaxKind Kind { get; }
    public int FullWidth { get; }
    public int SlotCount { get; }
    public int StartPosition { get; }
    public int EndPosition => StartPosition + FullWidth;
    public IEnumerable<DiagnosticInfo> Diagnostics { get; }

    protected GreenNode(SyntaxKind kind, int slotCount, int fullWidth, IEnumerable<DiagnosticInfo> diagnostics = null, int startPosition = 0)
    {
        Kind = kind;
        SlotCount = slotCount;
        FullWidth = fullWidth;
        StartPosition = startPosition;
        Diagnostics = diagnostics ?? Enumerable.Empty<DiagnosticInfo>();
    }

    public abstract GreenNode GetSlot(int index);

    internal GreenNode? GetFirstTerminal()
    {
        GreenNode? node = this;

        GreenNode? firstChild = null;

        do
        {
            for (int i = 0, n = node.SlotCount; i < n; i++)
            {
                var child = node.GetSlot(i);
                if (child != null)
                {
                    firstChild = child;
                    break;
                }
            }
            node = firstChild;
        }
        while (node is not null and not InternalSyntax.SyntaxToken);

        return node;
    }

    internal GreenNode? GetLastTerminal()
    {
        GreenNode? node = this;

        GreenNode? lastChild = null;

        do
        {
            for (int i = node.SlotCount - 1; i >= 0; i--)
            {
                var child = node.GetSlot(i);
                if (child != null)
                {
                    lastChild = child;
                    break;
                }
            }
            node = lastChild;
        }
        while (node is not null and not InternalSyntax.SyntaxToken);

        return node;
    }

    public virtual Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return null!;
    }

    public virtual TNode GetRed<TNode>(ref TNode node, int index)
        where TNode : Syntax.SyntaxNode
    {
        if (node is not null)
        {
            return node;
        }

        var slot = GetSlot(index);
        if (slot is not null)
        {
            node = (TNode)slot.CreateRed(null);
            return node;
        }
        return null!;
    }
}