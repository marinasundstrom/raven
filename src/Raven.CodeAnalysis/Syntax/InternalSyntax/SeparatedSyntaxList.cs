namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SeparatedSyntaxList : GreenNode
{
    public static readonly SeparatedSyntaxList Empty = new([]);

    private readonly GreenNode[] _items;

    public SeparatedSyntaxList(GreenNode[] items)
        : base(SyntaxKind.List, items?.Length ?? 0, CalculateWidth(items), CalculateFullWidth(items))
    {
        _items = items ?? Array.Empty<GreenNode>();
    }

    public override GreenNode GetSlot(int index) => _items[index];

    public GreenNode this[int index] => _items[index];

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return new SeparatedSyntaxList(newChildren);
    }
}