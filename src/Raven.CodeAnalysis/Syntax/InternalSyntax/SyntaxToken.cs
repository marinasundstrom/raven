using System.Diagnostics;
using System.Linq;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal class SyntaxToken : GreenNode
{
    private readonly string _text;
    private readonly object? _value;
    private bool _isMissing;

    public string Text => _text;

    internal InternalSyntax.SyntaxTriviaList LeadingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;
    internal InternalSyntax.SyntaxTriviaList TrailingTrivia { get; set; } = InternalSyntax.SyntaxTriviaList.Empty;

    public SyntaxToken(
        SyntaxKind kind,
        string text,
        SyntaxTriviaList leadingTrivia = null,
        SyntaxTriviaList trailingTrivia = null,
        IEnumerable<DiagnosticInfo>? diagnostics = null,
        IEnumerable<SyntaxAnnotation>? annotations = null)
    : this(kind, text, null, text.Length, leadingTrivia, trailingTrivia, diagnostics, annotations)
    {

    }

    public SyntaxToken(
        SyntaxKind kind,
        string text,
        object? value,
        int width,
        SyntaxTriviaList? leadingTrivia = null,
        SyntaxTriviaList? trailingTrivia = null,
        IEnumerable<DiagnosticInfo>? diagnostics = null,
        IEnumerable<SyntaxAnnotation>? annotations = null)
        : base(kind, 0, diagnostics, annotations)
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

    public override string? GetValueText() => _value?.ToString() ?? _text;

    public SyntaxToken WithLeadingTrivia(params IEnumerable<SyntaxTrivia> trivias)
    {
        var triviaArray = trivias?.ToArray() ?? Array.Empty<SyntaxTrivia>();
        return WithLeadingTrivia(SyntaxFactory.TriviaList(triviaArray));
    }

    public SyntaxToken WithTrailingTrivia(params IEnumerable<SyntaxTrivia> trivias)
    {
        var triviaArray = trivias?.ToArray() ?? Array.Empty<SyntaxTrivia>();
        return WithTrailingTrivia(SyntaxFactory.TriviaList(triviaArray));
    }

    public SyntaxToken WithLeadingTrivia(SyntaxTriviaList triviaList)
    {
        return new SyntaxToken(Kind, _text, _value, Width, triviaList, TrailingTrivia, _diagnostics, _annotations);
    }

    public SyntaxToken WithTrailingTrivia(SyntaxTriviaList triviaList)
    {
        return new SyntaxToken(Kind, _text, _value, Width, LeadingTrivia, triviaList, _diagnostics, _annotations);
    }

    public override int GetLeadingTriviaWidth() => LeadingTrivia.Width;

    public override int GetTrailingTriviaWidth() => TrailingTrivia.Width;

    internal static SyntaxToken Missing(SyntaxKind kind)
    {
        return new SyntaxToken(kind, string.Empty) { _isMissing = true };
    }

    public static explicit operator Syntax.SyntaxToken(InternalSyntax.SyntaxToken token)
    {
        return new Syntax.SyntaxToken(token, null!);
    }

    internal override GreenNode With(GreenNode[] children, DiagnosticInfo[]? diagnostics = null, SyntaxAnnotation[]? annotations = null)
    {
        if (diagnostics is null && annotations is null)
        {
            return this;
        }

        return new SyntaxToken(Kind, _text, _value, Width, LeadingTrivia, TrailingTrivia, diagnostics ?? _diagnostics, annotations ?? _annotations);
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
        return new SyntaxToken(Kind, _text, _value, Width, LeadingTrivia, TrailingTrivia, diagnostics, _annotations);
    }

    private string GetDebuggerDisplay()
    {
        if (string.IsNullOrEmpty(Text))
        {
            return $"{GetType().Name} {Kind} {(IsMissing ? "<missing>" : string.Empty)}";
        }
        return $"{GetType().Name} {Kind} {(IsMissing ? "<missing>" : Text)}";
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
