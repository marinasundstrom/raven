using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Raven.CodeAnalysis.Syntax;

[StructLayout(LayoutKind.Auto)]
[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public struct SyntaxToken : IEquatable<SyntaxToken>
{
    internal readonly InternalSyntax.SyntaxToken Green;
    private readonly SyntaxNode _parent;

    public string Text => Green.Text;

    public bool IsMissing => Green.IsMissing;

    public int Width => Green.Width;
    public int FullWidth => Green.FullWidth;

    public SyntaxTriviaList LeadingTrivia => new SyntaxTriviaList(this, Green.LeadingTrivia, StartPosition);
    public SyntaxTriviaList TrailingTrivia => new SyntaxTriviaList(this, Green.TrailingTrivia, StartPosition + Width);

    internal SyntaxToken(InternalSyntax.SyntaxToken greenToken, SyntaxNode parent, int position = 0)
    {
        Green = greenToken;
        _parent = parent;
        StartPosition = position;
    }

    private string GetDebuggerDisplay()
    {
        return GetType().Name + " " + (Green != null ? Green.Text : "None") + " " + ToString();
    }

    public SyntaxKind Kind => Green.Kind;

    public object? Value => Green.GetValue();

    public string? ValueText => Green.GetValueText();

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

    public int StartPosition { get; }

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
            return StartPosition + LeadingTrivia.Width;
        }
    }

    public TextSpan FullSpan
    {
        get
        {
            return new TextSpan(StartPosition, Green.FullWidth);
        }
    }

    public override string ToString()
    {
        return this.Text;
    }

    public string ToFullString()
    {
        return this.LeadingTrivia.ToString() + this.Text + this.TrailingTrivia.ToString();
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

    public Location GetLocation()
    {
        if (SyntaxTree is null)
        {
            return default(Location)!;
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

    public bool ContainsDiagnostics => SyntaxTree?.GetDiagnostics(this).Any() ?? false;

    public IEnumerable<Diagnostic> GetDiagnostics()
    {
        return SyntaxTree?.GetDiagnostics(this) ?? Enumerable.Empty<Diagnostic>();
    }

    internal SyntaxToken Accept(SyntaxRewriter syntaxRewriter)
    {
        return syntaxRewriter.VisitToken(this);
    }
}
