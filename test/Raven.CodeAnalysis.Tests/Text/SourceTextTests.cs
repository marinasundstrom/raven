using System.IO;

using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class SourceTextTests
{
    [Fact]
    public void GetTextReader_AsciiOffset_DoesNotRewind()
    {
        const string text = "prefix\"value\"";
        var start = text.IndexOf('"');
        var sourceText = SourceText.From(text);

        using var reader = sourceText.GetTextReader(start);
        var sliced = reader.ReadToEnd();

        Assert.Equal(text[start..], sliced);
    }

    [Fact]
    public void GetTextReader_LowSurrogateOffset_RewindsToHighSurrogate()
    {
        const string text = "🙂🙂\"value\"";
        var lowSurrogateIndex = 1; // Inside the first emoji surrogate pair.
        var sourceText = SourceText.From(text);

        using var reader = sourceText.GetTextReader(lowSurrogateIndex);
        var sliced = reader.ReadToEnd();

        Assert.Equal(text, sliced);
    }

    [Fact]
    public void Replace_RetainsDirectChangeRange()
    {
        var original = SourceText.From("abcdef");
        var updated = original.Replace(new TextSpan(2, 2), "XYZ");

        var range = Assert.Single(updated.GetChangeRanges(original));
        Assert.Equal(new TextSpan(2, 2), range.Span);
        Assert.Equal(3, range.NewLength);

        var change = Assert.Single(updated.GetTextChanges(original));
        Assert.Equal(new TextSpan(2, 2), change.Span);
        Assert.Equal("XYZ", change.NewText);
    }

    [Fact]
    public void ChainedReplace_ComputesChangeRangeAgainstOriginalText()
    {
        var original = SourceText.From("abcdef");
        var intermediate = original.Replace(new TextSpan(1, 1), "B");
        var updated = intermediate.Replace(new TextSpan(4, 1), "E");

        var range = Assert.Single(updated.GetChangeRanges(original));
        Assert.Equal(new TextSpan(1, 4), range.Span);
        Assert.Equal(4, range.NewLength);

        Assert.Equal("aBcdEf", updated.ToString());
    }
}
