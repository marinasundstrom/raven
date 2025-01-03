namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxTrivia : GreenNode
{
    private readonly SyntaxNode? _structuredTrivia;

    public string Text { get; }

    public SyntaxTrivia(
        SyntaxKind kind,
        string text,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, 0, diagnostics)
    {
        Text = text;

        Width = text.Length;
        FullWidth = text.Length;
    }

    public SyntaxTrivia(
        SyntaxNode node,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(node.Kind, 0, diagnostics)
    {
        _structuredTrivia = node;
        Text = string.Empty;

        Width = node.Width;
        FullWidth = node.FullWidth;
    }

    public bool HasStructuredTrivia => _structuredTrivia is not null;

    public SyntaxNode? GetStructuredTrivia() => _structuredTrivia;

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxTrivia has no children.");

    public static explicit operator Syntax.SyntaxTrivia(InternalSyntax.SyntaxTrivia trivia)
    {
        return new Syntax.SyntaxTrivia(trivia, default!);
    }

    internal override IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive()
    {
        if (_diagnostics is not null)
        {
            foreach (var diagnostic in _diagnostics)
            {
                yield return diagnostic;
            }
        }
    }

    internal override GreenNode WithDiagnostics(params DiagnosticInfo[] diagnostics)
    {
        return new SyntaxTrivia(Kind, Text, _diagnostics);
    }

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return this;
    }
}

internal static partial class SyntaxFactory
{
    public static SyntaxTrivia Trivia(
        SyntaxKind kind,
        string text,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(kind, text, diagnostics);

    public static SyntaxTrivia Trivia(
        SyntaxNode structure,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(structure, diagnostics);
}
