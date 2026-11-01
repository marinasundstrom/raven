using System.Text;

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "DotNet")]
public sealed class StringLiteralDecodingTests
{
    [Theory]
    [InlineData("", "", false)]
    [InlineData("plain λ", "plain λ", false)]
    [InlineData(@"\0\a\b\f\n\r\t\v", "\0\a\b\f\n\r\t\v", false)]
    [InlineData("\\\"\\'\\\\\\$", "\"'\\$", false)]
    [InlineData(@"\u0041\U0001F600\u{1F642}", "A😀🙂", false)]
    [InlineData(@"\qend", "?end", true)]
    [InlineData("trailing\\", "trailing?", true)]
    [InlineData(@"\uD800", "?", true)]
    [InlineData(@"\u{110000}", "?}", true)]
    public void DecodePreservesTextAndReportsInvalidEscapes(string input, string expected, bool invalid)
    {
        Assert.Equal(expected, SyntaxFacts.DecodeStringLiteralContent(input.AsSpan(), out var actualInvalid));
        Assert.Equal(invalid, actualInvalid);
    }

    [Theory]
    [InlineData("0041tail", 4, false, true, 65, 4)]
    [InlineData("0001F600tail", 8, false, true, 0x1F600, 8)]
    [InlineData("{41}tail", 4, true, true, 65, 4)]
    [InlineData("{}", 4, true, false, 0, 2)]
    [InlineData("12", 4, false, false, 0, 2)]
    [InlineData("12G4", 4, false, false, 0, 3)]
    [InlineData("D800", 4, false, false, 0, 4)]
    [InlineData("00110000", 8, false, false, 0, 8)]
    [InlineData("{41", 4, true, false, 0, 3)]
    [InlineData("{000000000}", 4, true, false, 0, 10)]
    public void UnicodeDecodingReportsScalarAndConsumedWidth(string input, int digits, bool braces, bool success, int scalar, int consumed)
    {
        Assert.Equal(success, SyntaxFacts.TryDecodeUnicodeEscape(input.AsSpan(), digits, braces, out var rune, out var width));
        Assert.Equal(new Rune(scalar), rune);
        Assert.Equal(consumed, width);
    }

    [Fact]
    public void HexHelpersAcceptOnlyHexadecimalDigits()
    {
        foreach (var ch in "0123456789abcdefABCDEF")
        {
            Assert.True(SyntaxFacts.IsHexDigit(ch));
            Assert.Equal(Convert.ToInt32(ch.ToString(), 16), SyntaxFacts.GetHexValue(ch));
        }
        foreach (var ch in "gG/- λ")
        {
            Assert.False(SyntaxFacts.IsHexDigit(ch));
            Assert.Throws<ArgumentOutOfRangeException>(() => SyntaxFacts.GetHexValue(ch));
        }
    }
}
