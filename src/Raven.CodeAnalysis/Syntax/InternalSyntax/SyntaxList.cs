namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxList : GreenNode
{
    public static readonly SyntaxList Empty = new([]);

    private readonly GreenNode[] _items;

    public SyntaxList(GreenNode[] items,
        IEnumerable<Diagnostic>? diagnostics = null)
        : base(SyntaxKind.List, items?.Length ?? 0, diagnostics)
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

    internal override GreenNode WithDiagnostics(params Diagnostic[] diagnostics)
    {
        return new SyntaxList(_items, _diagnostics);
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

    internal override IEnumerable<Diagnostic> GetDiagnostics()
    {
        foreach (var child in GetChildren())
        {
            foreach (var diagnostic in child.GetDiagnostics())
            {
                yield return diagnostic;
            }
        }
    }
}

internal static partial class SyntaxFactory
{
    public static SyntaxList List(
        IEnumerable<GreenNode> items,
        IEnumerable<Diagnostic>? diagnostics = null)
      => new(items.ToArray(), diagnostics);
}
