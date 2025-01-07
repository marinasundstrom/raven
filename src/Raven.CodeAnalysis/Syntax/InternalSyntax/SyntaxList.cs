namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxList : GreenNode
{
    public static readonly SyntaxList Empty = new([]);

    private readonly GreenNode[] _items;

    public SyntaxList(GreenNode[] items,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.List, items?.Length ?? 0, diagnostics)
    {
        _items = items ?? throw new ArgumentNullException(nameof(items));

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

    internal override GreenNode SetDiagnostics(params DiagnosticInfo[] diagnostics)
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

    internal override IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive()
    {
        foreach (var child in GetChildren())
        {
            foreach (var diagnostic in child.GetDiagnosticsRecursive())
            {
                yield return diagnostic;
            }
        }
    }

    internal override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitSyntaxList(this);
    }

    internal override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
    {
        return visitor.VisitSyntaxList(this);
    }
}

internal static partial class SyntaxFactory
{
    public static SyntaxList List(
        IEnumerable<GreenNode> items,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(items.ToArray(), diagnostics);
}
