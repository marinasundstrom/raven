using System.IO;

using Raven.CodeAnalysis.Text;

using Xunit;

namespace Raven.CodeAnalysis.Text.Tests;

public class SourceTextTests
{
    [Fact]
    public void Indexer_ReturnsCharacterWithoutMaterializingSubText()
    {
        var sourceText = SourceText.From("Raven");

        Assert.Equal('R', sourceText[0]);
        Assert.Equal('n', sourceText[sourceText.Length - 1]);
    }

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

    [Fact]
    public void CopyTo_CopiesRequestedCharacters()
    {
        var sourceText = SourceText.From("Raven");
        var destination = ".....".ToCharArray();

        sourceText.CopyTo(1, destination, 2, 3);

        Assert.Equal("..ave", new string(destination));
    }

    [Fact]
    public void GetLines_ReportsTextAndLineBreakSpans()
    {
        var sourceText = SourceText.From("first\r\nsecond\n");

        var lines = sourceText.GetLines();

        Assert.Same(lines, sourceText.GetLines());
        Assert.Equal(3, lines.Count);
        Assert.Collection(
            lines,
            line =>
            {
                Assert.Equal(0, line.LineNumber);
                Assert.Equal(new TextSpan(0, 5), line.Span);
                Assert.Equal(new TextSpan(0, 7), line.SpanIncludingLineBreak);
                Assert.Equal("first", line.ToString());
            },
            line =>
            {
                Assert.Equal(1, line.LineNumber);
                Assert.Equal(new TextSpan(7, 6), line.Span);
                Assert.Equal(new TextSpan(7, 7), line.SpanIncludingLineBreak);
                Assert.Equal("second", line.ToString());
            },
            line =>
            {
                Assert.Equal(2, line.LineNumber);
                Assert.Equal(new TextSpan(14, 0), line.Span);
                Assert.Equal(line.Span, line.SpanIncludingLineBreak);
                Assert.Equal(string.Empty, line.ToString());
            });
    }

    [Fact]
    public void Write_WritesFullTextOrSpanWithoutChangingContent()
    {
        var sourceText = SourceText.From("Raven compiler");
        using var fullWriter = new StringWriter();
        using var spanWriter = new StringWriter();

        sourceText.Write(fullWriter, CancellationToken.None);
        sourceText.Write(spanWriter, new TextSpan(6, 8), CancellationToken.None);

        Assert.Equal("Raven compiler", fullWriter.ToString());
        Assert.Equal("compiler", spanWriter.ToString());
    }

    [Fact]
    public void Write_ObservesCancellationBeforeWriting()
    {
        var sourceText = SourceText.From("Raven");
        using var writer = new StringWriter();
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        Assert.Throws<OperationCanceledException>(() => sourceText.Write(writer, cancellation.Token));
        Assert.Equal(string.Empty, writer.ToString());
    }
}
