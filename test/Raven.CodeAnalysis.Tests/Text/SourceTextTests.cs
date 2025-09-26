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
}
