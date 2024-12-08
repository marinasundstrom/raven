namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SeparatedSyntaxList : GreenNode
{
    public static readonly SeparatedSyntaxList Empty = new([], 0);

    private readonly GreenNode[] _items;

    public SeparatedSyntaxList(GreenNode[] items, int startPosition = 0)
        : base(SyntaxKind.SyntaxList, items?.Length ?? 0, CalculateFullWidth(items), startPosition: startPosition)
    {
        _items = items ?? Array.Empty<GreenNode>();
    }

    public override GreenNode GetSlot(int index) => _items[index];

    public GreenNode this[int index] => _items[index];

    private static int CalculateFullWidth(GreenNode[] items) =>
        items?.Sum(item => item.FullWidth) ?? 0;
}