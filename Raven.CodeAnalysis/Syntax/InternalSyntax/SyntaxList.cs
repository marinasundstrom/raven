namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public class SyntaxList : GreenNode
{
    public static readonly SyntaxList Empty = new([]);

    private readonly SyntaxListItem[] _items;

    public SyntaxList(SyntaxListItem[] items)
        : base(SyntaxKind.SyntaxList, items?.Length ?? 0, CalculateWidth(items), CalculateFullWidth(items))
    {
        _items = items ?? Array.Empty<SyntaxListItem>();
    }

    public override GreenNode GetSlot(int index) => _items[index].Node;

    public SyntaxListItem this[int index] => _items[index];

    private static int CalculateWidth(SyntaxListItem[] items) =>
        items?.Sum(item => item.Node.Width) ?? 0;

    private static int CalculateFullWidth(SyntaxListItem[] items) =>
        items?.Sum(item => item.Node.FullWidth) ?? 0;
}

public readonly struct SyntaxListItem
{
    public GreenNode Node { get; }

    public SyntaxListItem(GreenNode node)
    {
        Node = node ?? throw new ArgumentNullException(nameof(node));
    }

    public bool IsToken => Node is SyntaxToken;
    public bool IsNode => Node is SyntaxNode;
    public SyntaxToken Token => Node as SyntaxToken;
    public SyntaxNode NodeSyntax => Node as SyntaxNode;
}