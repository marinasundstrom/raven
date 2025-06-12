namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxToken : GreenNode
{
    private readonly string _text;
    private readonly object? _value;
    private bool _isMissing;

    public string Text => _text;

    public SyntaxToken(
        SyntaxKind kind,
        string text,
        SyntaxTriviaList leadingTrivia = null,
        SyntaxTriviaList trailingTrivia = null,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
    : this(kind, text, null, text.Length, leadingTrivia, trailingTrivia, diagnostics)
    {

    }

    public SyntaxToken(
        SyntaxKind kind,
        string text,
        object? value,
        int width,
        SyntaxTriviaList? leadingTrivia = null,
        SyntaxTriviaList? trailingTrivia = null,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(kind, 0, diagnostics)
    {
        _text = text;
        _value = value;

        LeadingTrivia = leadingTrivia ?? SyntaxTriviaList.Empty;
        TrailingTrivia = trailingTrivia ?? SyntaxTriviaList.Empty;

        Width = width;
        FullWidth = (leadingTrivia?.FullWidth ?? 0) + width + (trailingTrivia?.FullWidth ?? 0);
    }

    public override bool IsMissing => _isMissing;

    public override GreenNode GetSlot(int index) => throw new InvalidOperationException("SyntaxToken has no children.");

    public override object? GetValue() => _value;

    public override string? GetValueText() => _value?.ToString();

    public SyntaxToken WithLeadingTrivia(params IEnumerable<SyntaxTrivia> trivias)
    {
        return new SyntaxToken(Kind, Text, SyntaxTriviaList.Create(trivias.ToArray()), TrailingTrivia); ;
    }

    public SyntaxToken WithTrailingTrivia(params IEnumerable<SyntaxTrivia> trivias)
    {
        return new SyntaxToken(Kind, Text, LeadingTrivia, SyntaxTriviaList.Create(trivias.ToArray())); ;
    }

    internal static SyntaxToken Missing(SyntaxKind kind)
    {
        return new SyntaxToken(kind, string.Empty) { _isMissing = true };
    }

    public static explicit operator Syntax.SyntaxToken(InternalSyntax.SyntaxToken token)
    {
        return new Syntax.SyntaxToken(token, null!);
    }

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return this;
    }

    internal override IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive()
    {
        foreach (var diagnostic in LeadingTrivia.GetDiagnosticsRecursive())
        {
            yield return diagnostic;
        }

        if (_diagnostics is not null)
        {
            foreach (var diagnostic in _diagnostics)
            {
                yield return diagnostic;
            }
        }

        foreach (var diagnostic in TrailingTrivia.GetDiagnosticsRecursive())
        {
            yield return diagnostic;
        }
    }

    internal override GreenNode SetDiagnostics(params DiagnosticInfo[] diagnostics)
    {
        return new SyntaxToken(Kind, _text, _value, Width, LeadingTrivia, TrailingTrivia, _diagnostics);
    }

    internal override void Accept(SyntaxVisitor visitor)
    {
        visitor.VisitToken(this);
    }

    internal override TResult Accept<TResult>(SyntaxVisitor<TResult> visitor)
    {
        return visitor.VisitToken(this);
    }
}