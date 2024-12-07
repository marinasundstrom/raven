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

    public SyntaxToken(InternalSyntax.SyntaxToken greenToken, SyntaxNode parent)
    {

        Green = greenToken;
        _parent = parent;
    }

    private string GetDebuggerDisplay()
    {
        return GetType().Name + " " + (Green != null ? Green.Text : "None") + " " + ToString();
    }

    public bool Equals(SyntaxToken other)
    {
        return other.Kind == other.Kind;
    }

    // Additional properties or methods specific to SyntaxToken

    public SyntaxKind Kind => Green.Kind;

    public SyntaxNode Parent => _parent;

    public static explicit operator SyntaxToken(InternalSyntax.SyntaxToken token)
    {
        return new SyntaxToken(token, null!);
    }
}