using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Raven.CodeAnalysis.Syntax;

[StructLayout(LayoutKind.Auto)]
[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public struct SyntaxToken : IEquatable<SyntaxToken>
{
    internal readonly InternalSyntax.SyntaxToken Green;
    private readonly SyntaxNode _parent;

    public string Text => Green?.Text ?? string.Empty;

    public bool IsMissing => Green?.IsMissing ?? true;

    public int Width => Green?.Width ?? 0;
    public int FullWidth => Green?.FullWidth ?? 0;

    public bool HasLeadingTrivia => Green?.LeadingTrivia.Count > 0;
    public bool HasTrailingTrivia => Green?.TrailingTrivia.Count > 0;

    public SyntaxTriviaList LeadingTrivia => Green is null
        ? SyntaxTriviaList.Empty
        : new(this, Green.LeadingTrivia, Position);

    public SyntaxTriviaList TrailingTrivia => Green is null
        ? SyntaxTriviaList.Empty
        : new(this, Green.TrailingTrivia, Position + Width);

    internal SyntaxToken(InternalSyntax.SyntaxToken greenToken, SyntaxNode? parent, int position = 0)
    {
        Green = greenToken;
        _parent = parent;
        Position = position;
    }
    private string GetDebuggerDisplay()
    {
        if (string.IsNullOrEmpty(Text))
        {
            return $"{GetType().Name} {Kind}";
        }
        return $"{GetType().Name} {Kind} {Text ?? Value?.ToString()}";
    }

    public SyntaxKind Kind => Green.Kind;

    public object? Value => Green?.GetValue();

    public string ValueText => Green?.GetValueText() ?? Text;

    public SyntaxNode Parent => _parent;

    public SyntaxTree? SyntaxTree
    {
        get
        {
            if (_parent is not null)
            {
                return _parent.SyntaxTree;
            }

            return null;
        }
    }

    internal int Position { get; }

    internal int End => Position + FullWidth;

    public TextSpan Span
    {
        get
        {
            return new TextSpan(SpanStart, Green.Width);
        }
    }

    public int SpanStart
    {
        get
        {
            return Position + LeadingTrivia.Width;
        }
    }

    public TextSpan FullSpan
    {
        get
        {
            return new TextSpan(Position, Green.FullWidth);
        }
    }

    public override string ToString()
    {
        return Text;
    }

    public string ToFullString()
    {
        return LeadingTrivia.ToString() + Text + TrailingTrivia.ToString();
    }

    public SyntaxToken WithLeadingTrivia(params IEnumerable<SyntaxTrivia> trivias)
    {
        var newGreen = Green.WithLeadingTrivia(trivias.Select(x => x.Green));
        return new SyntaxToken(newGreen, Parent);
    }

    public SyntaxToken WithTrailingTrivia(params IEnumerable<SyntaxTrivia> trivias)
    {
        var newGreen = Green.WithTrailingTrivia(trivias.Select(x => x.Green));
        return new SyntaxToken(newGreen, Parent);
    }

    public SyntaxToken WithLeadingTrivia(SyntaxTriviaList trivialList)
    {
        var newGreen = Green.WithLeadingTrivia(trivialList.Green);
        return new SyntaxToken(newGreen, Parent);
    }

    public SyntaxToken WithTrailingTrivia(SyntaxTriviaList trivialList)
    {
        var newGreen = Green.WithTrailingTrivia(trivialList.Green);
        return new SyntaxToken(newGreen, Parent);
    }

    public Location GetLocation()
    {
        if (SyntaxTree is null)
        {
            return Location.None;
        }
        return SyntaxTree!.GetLocation(Span);
    }

    public static bool operator ==(SyntaxToken left, SyntaxToken? right) => Equals(left, right);

    public static bool operator !=(SyntaxToken left, SyntaxToken? right) => !Equals(left, right);

    public static bool operator ==(SyntaxToken? left, SyntaxToken? right) => Equals(left, right);

    public static bool operator !=(SyntaxToken? left, SyntaxToken? right) => !Equals(left, right);

    public bool Equals(SyntaxToken other)
    {
        return Green == other.Green && _parent == other._parent;
    }

    public override bool Equals(object? obj)
    {
        return obj is SyntaxToken other && Equals(other);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Green, _parent);
    }

    public bool ContainsDiagnostics => GetDiagnostics().Any();

    public IEnumerable<Diagnostic> GetDiagnostics()
    {
        return SyntaxTree?.GetDiagnostics(FullSpan) ?? Enumerable.Empty<Diagnostic>();
    }

    internal SyntaxToken Accept(SyntaxRewriter syntaxRewriter)
    {
        return syntaxRewriter.VisitToken(this);
    }
}
