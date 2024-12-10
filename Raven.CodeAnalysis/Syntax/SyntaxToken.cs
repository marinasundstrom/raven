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

    public SyntaxTriviaList LeadingTrivia => new SyntaxTriviaList(this, Green.LeadingTrivia);
    public SyntaxTriviaList TrailingTrivia => new SyntaxTriviaList(this, Green.TrailingTrivia);

    public SyntaxToken(InternalSyntax.SyntaxToken greenToken, SyntaxNode parent, int position = 0)
    {

        Green = greenToken;
        _parent = parent;
        StartPosition = position;
    }

    private string GetDebuggerDisplay()
    {
        return GetType().Name + " " + (Green != null ? Green.Text : "None") + " " + ToString();
    }

    // Additional properties or methods specific to SyntaxToken

    public SyntaxKind Kind => Green.Kind;

    public SyntaxNode Parent => _parent;

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

    public static explicit operator SyntaxToken(InternalSyntax.SyntaxToken token)
    {
        return new SyntaxToken(token, null!);
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
}