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

    public SyntaxTriviaList LeadingTrivia => new SyntaxTriviaList(this, Green.LeadingTrivia, Position);
    public SyntaxTriviaList TrailingTrivia => new SyntaxTriviaList(this, Green.TrailingTrivia, Position + Width);

    internal SyntaxToken(InternalSyntax.SyntaxToken greenToken, SyntaxNode parent, int position = 0)
    {
        Green = greenToken;
        _parent = parent;
        Position = position;
    }
    private string GetDebuggerDisplay()
    {
        return $"{Kind} \"{Text}\"{(Value is not null ? $" = {Value}" : "")}";
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

    public int Position { get; }

    public int End => Position + FullWidth;

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
