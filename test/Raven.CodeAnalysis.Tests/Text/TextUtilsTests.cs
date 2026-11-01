using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Text;

[Trait("Target", "Shared")]
public sealed class TextUtilsTests
{
    [Theory]
    [InlineData("", new[] { 0 })]
    [InlineData("abc", new[] { 0 })]
    [InlineData("a\nb", new[] { 0, 2 })]
    [InlineData("a\rb", new[] { 0, 2 })]
    [InlineData("a\r\nb", new[] { 0, 3 })]
    [InlineData("a\r\nb\nc\r", new[] { 0, 3, 5, 7 })]
    [InlineData("\r\r\n\n", new[] { 0, 1, 3, 4 })]
    [InlineData("🙂\nx", new[] { 0, 3 })]
    public void ComputeLineStarts_RecognizesLineBreaks(string text, int[] expected)
    {
        Assert.Equal(expected, TextUtils.ComputeLineStarts(text));
    }

    [Theory]
    [InlineData(0, 1, 1)]
    [InlineData(1, 1, 2)]
    [InlineData(2, 1, 3)]
    [InlineData(3, 2, 1)]
    [InlineData(5, 2, 3)]
    [InlineData(6, 3, 1)]
    public void GetLineAndColumn_UsesOneBasedCoordinates(int position, int line, int column)
    {
        var starts = TextUtils.ComputeLineStarts("a\r\nbc\n");
        var actual = TextUtils.GetLineAndColumn(starts, position);
        Assert.Equal(line, actual.line);
        Assert.Equal(column, actual.column);
    }
}
