using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class TokenValueTests
{
    [Fact]
    public void IdentifierToken_InternsTextAndRetainsUtf16Width()
    {
        var text = new string("name_å".ToCharArray());
        var token = new Token(SyntaxKind.IdentifierToken, text);
        var other = new Token(SyntaxKind.IdentifierToken, new string(text.ToCharArray()));
        Assert.Equal(SyntaxKind.IdentifierToken, token.Kind);
        Assert.Equal(text, token.Text);
        Assert.Equal(text.Length, token.Length);
        Assert.Null(token.Value);
        Assert.Same(token.Text, other.Text);
        Assert.Equal(token, other);
        Assert.Equal(token.GetHashCode(), other.GetHashCode());
    }

    [Fact]
    public void ValueToken_PreservesPayloadAndAuthoredWidth()
    {
        var token = new Token(SyntaxKind.IdentifierToken, "value", 42);
        var same = new Token(SyntaxKind.IdentifierToken, "value", 42);
        var wider = new Token(SyntaxKind.IdentifierToken, "value", 42, 12);
        Assert.Equal(42, token.Value);
        Assert.Equal(5, token.Length);
        Assert.Equal(12, wider.Length);
        Assert.Equal("value", wider.Text);
        Assert.Equal(token, same);
        Assert.Equal(token.GetHashCode(), same.GetHashCode());
        Assert.NotEqual(token, wider);
        Assert.NotEqual(token, new Token(SyntaxKind.IdentifierToken, "value", 43));
    }

    [Fact]
    public void DefaultToken_IsEmpty()
    {
        var token = default(Token);
        Assert.Equal(SyntaxKind.None, token.Kind);
        Assert.Null(token.Text);
        Assert.Null(token.Value);
        Assert.Equal(0, token.Length);
    }
}
