using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class AnsiExtensionsTests
{
    [Fact]
    public void StripAnsiCodes_RemovesEscapeSequences()
    {
        var text = "\u001b[31mhello\u001b[0m";
        var result = text.StripAnsiCodes();
        Assert.Equal("hello", result);
    }

    [Theory]
    [InlineData("plain text", "plain text")]
    [InlineData("\u001b[38;2;12;34;56mcolored\u001b[0m", "colored")]
    [InlineData("\u001b[38:2:12:34:56mcolored", "colored")]
    [InlineData("left\u001b[mright", "leftright")]
    [InlineData("\u001b[2Jnot a color", "\u001b[2Jnot a color")]
    [InlineData("unfinished\u001b[31", "unfinished\u001b[31")]
    public void StripAnsiCodes_OnlyRemovesCompleteColorSequences(string input, string expected)
    {
        Assert.Equal(expected, input.StripAnsiCodes());
        Assert.Equal(expected, AnsiExtensions.StripAnsiCodes(input));
    }

    [Fact]
    public void AnsiColor_UsesStandardSgrCodes()
    {
        Assert.Equal(0, (int)AnsiColor.Reset);
        Assert.Equal(Enumerable.Range(30, 8), new[] {
            AnsiColor.Black, AnsiColor.Red, AnsiColor.Green, AnsiColor.Yellow,
            AnsiColor.Blue, AnsiColor.Magenta, AnsiColor.Cyan, AnsiColor.White
        }.Select(color => (int)color));
        Assert.Equal(Enumerable.Range(90, 8), new[] {
            AnsiColor.BrightBlack, AnsiColor.BrightRed, AnsiColor.BrightGreen, AnsiColor.BrightYellow,
            AnsiColor.BrightBlue, AnsiColor.BrightMagenta, AnsiColor.BrightCyan, AnsiColor.BrightWhite
        }.Select(color => (int)color));
    }

}
