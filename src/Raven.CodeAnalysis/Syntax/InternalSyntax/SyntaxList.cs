namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxList : GreenNode
{
    public static readonly SyntaxList Empty = new([]);

    private readonly GreenNode[] _items;

    public SyntaxList(GreenNode[] items)
        : base(SyntaxKind.List, items?.Length ?? 0)
    {
        _items = items ?? Array.Empty<GreenNode>();

        Width = this.CalculateWidth();
        FullWidth = this.CalculateFullWidth();
    }

    private static int CalculateFullWidth(GreenNode[] items) =>
     items?.Sum(item => item.FullWidth) ?? 0;

    public override GreenNode GetSlot(int index) => _items[index];

    public GreenNode this[int index] => _items[index];

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return new SyntaxList(newChildren);
    }

    public SyntaxList Add(GreenNode green)
    {
        var list = _items.ToList();
        list.Add(green);
        return new SyntaxList(list.ToArray());
    }

    public SyntaxList Remove(GreenNode green)
    {
        var list = _items.ToList();
        list.Remove(green);
        return new SyntaxList(list.ToArray());
    }
}