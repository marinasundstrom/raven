namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxList : GreenNode
{
    public static readonly SyntaxList Empty = new([]);

    private readonly GreenNode[] _items;

    public SyntaxList(GreenNode[] items)
        : base(SyntaxKind.List, items?.Length ?? 0, CalculateFullWidth(items), CalculateFullWidth(items))
    {
        _items = items ?? Array.Empty<GreenNode>();
    }

    private static int CalculateFullWidth(GreenNode[] items) =>
     items?.Sum(item => item.FullWidth) ?? 0;

    public override GreenNode GetSlot(int index) => _items[index];

    public GreenNode this[int index] => _items[index];
}