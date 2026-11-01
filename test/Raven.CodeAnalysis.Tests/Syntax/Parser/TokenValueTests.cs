using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class TokenValueTests
{
    [Fact]
    public void GraphemePayloadPreservesTextAndValueEquality()
    {
        var first = new GraphemeLiteralValue("e\u0301");
        var same = new GraphemeLiteralValue("e\u0301");
        Assert.Equal(first, same);
        Assert.Equal(first.GetHashCode(), same.GetHashCode());
        Assert.NotEqual(first, new GraphemeLiteralValue("e"));
        Assert.Equal("e\u0301", first.ToString());
        Assert.Equal(first.Text, first.ToString());
    }

    [Fact]
    public void EncodedPayloadHasValueEqualityAcrossAllSemanticFields()
    {
        var first = new EncodedStringLiteralValue("hello", EncodedStringLiteralEncoding.Utf8, false);
        var same = new EncodedStringLiteralValue(new string("hello".ToCharArray()), EncodedStringLiteralEncoding.Utf8, false);
        Assert.NotSame(first, same);
        Assert.Equal(first, same);
        Assert.Equal(first.GetHashCode(), same.GetHashCode());
        Assert.True(first == same);
        Assert.False(first != same);
        Assert.IsAssignableFrom<IEquatable<EncodedStringLiteralValue>>(first);
        Assert.False(first.Equals(null));
        Assert.False(first.Equals(new GraphemeLiteralValue("hello")));
        Assert.True(typeof(EncodedStringLiteralValue).IsSealed);
        Assert.False(typeof(EncodedStringLiteralValue).IsValueType);
        foreach (var different in new[]
        {
            new EncodedStringLiteralValue("HELLO", EncodedStringLiteralEncoding.Utf8, false),
            new EncodedStringLiteralValue("hello", EncodedStringLiteralEncoding.Ascii, false),
            new EncodedStringLiteralValue("hello", EncodedStringLiteralEncoding.Utf8, true)
        })
        {
            Assert.NotEqual(first, different);
            Assert.True(first != different);
        }
        Assert.Single(new HashSet<EncodedStringLiteralValue> { first, same });
    }

    [Fact]
    public void TokenEqualityUsesEncodedPayloadValueSemantics()
    {
        var first = new Token(SyntaxKind.StringLiteralToken, "\"hello\"u8", new EncodedStringLiteralValue("hello", EncodedStringLiteralEncoding.Utf8, false));
        var same = new Token(SyntaxKind.StringLiteralToken, "\"hello\"u8", new EncodedStringLiteralValue("hello", EncodedStringLiteralEncoding.Utf8, false));
        Assert.Equal(first, same);
        Assert.Equal(first.GetHashCode(), same.GetHashCode());
        Assert.NotEqual(first, new Token(SyntaxKind.StringLiteralToken, first.Text,
            new EncodedStringLiteralValue("hello", EncodedStringLiteralEncoding.Ascii, false)));
        var payload = Assert.IsType<EncodedStringLiteralValue>(first.Value);
        Assert.Equal("hello", payload.Text);
        Assert.Equal(EncodedStringLiteralEncoding.Utf8, payload.Encoding);
        Assert.False(payload.ContainsInterpolation);
    }

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
