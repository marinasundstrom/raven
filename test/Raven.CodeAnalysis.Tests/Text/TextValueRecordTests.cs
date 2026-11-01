using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Text;

[Trait("Target", "Shared")]
public sealed class TextValueRecordTests
{
    [Fact]
    public void FileLinePositionSpan_UsesPathAndBothCoordinatesAsItsValue()
    {
        var start = new LinePosition(1, 2);
        var end = new LinePosition(3, 4);
        var value = new FileLinePositionSpan("source.rvn", start, end);
        var equal = new FileLinePositionSpan(new string("source.rvn".ToCharArray()), start, end);
        AssertValueEquality(value, equal, value with { Path = "other.rvn" });
        Assert.True(value == equal);
        Assert.True(value != value with { EndLinePosition = start });
        Assert.True(value != value with { StartLinePosition = end });
        var (path, actualStart, actualEnd) = value;
        Assert.Equal("source.rvn", path);
        Assert.Equal(start, actualStart);
        Assert.Equal(end, actualEnd);
        Assert.Equal("FileLinePositionSpan { Path = source.rvn, StartLinePosition = Line 1, Character 2, EndLinePosition = Line 3, Character 4 }", value.ToString());
    }

    [Fact]
    public void TextChange_UsesSpanAndReplacementTextAsItsValue()
    {
        var span = new TextSpan(2, 3);
        var value = new TextChange(span, "abc");
        var equal = new TextChange(span, new string("abc".ToCharArray()));
        AssertValueEquality(value, equal, value with { NewText = "xyz" });
        Assert.True(value == equal);
        Assert.True(value != value with { Span = new TextSpan(2, 4) });
        var (actualSpan, text) = value;
        Assert.Equal(span, actualSpan);
        Assert.Equal("abc", text);
        Assert.Equal("Span: [2..5], NewText: abc", value.ToString());
    }

    [Fact]
    public void TextChangeRange_UsesSpanAndNewLengthAsItsValue()
    {
        var span = new TextSpan(2, 3);
        var value = new TextChangeRange(5, span);
        var equal = new TextChangeRange(5, span);
        AssertValueEquality(value, equal, value with { NewLength = 6 });
        Assert.True(value == equal);
        Assert.True(value != value with { Span = new TextSpan(1, 3) });
        var (length, actualSpan) = value;
        Assert.Equal(5, length);
        Assert.Equal(span, actualSpan);
        Assert.Equal("Span: [2..5], NewLength: 5", value.ToString());
    }

    [Fact]
    public void EmptyChangeCollections_CannotBeMutatedByConsumers()
    {
        var changes = default(TextChange).NoChanges;
        var ranges = default(TextChangeRange).NoChanges;
        Assert.Empty(changes);
        Assert.Empty(ranges);
        Assert.Same(changes, new TextChange(default, "x").NoChanges);
        Assert.Same(ranges, new TextChangeRange(1, default).NoChanges);
        Assert.Throws<NotSupportedException>(() => ((ICollection<TextChange>)changes).Add(default));
        Assert.Throws<NotSupportedException>(() => ((ICollection<TextChangeRange>)ranges).Add(default));
    }

    private static void AssertValueEquality<T>(T value, T equal, T different) where T : struct, IEquatable<T>
    {
        Assert.True(value.Equals(equal));
        Assert.True(value.Equals((object)equal));
        Assert.False(value.Equals((object)different));
        Assert.False(value.Equals(null));
        Assert.Equal(value.GetHashCode(), equal.GetHashCode());
        Assert.Single(new HashSet<T> { value, equal });
        Assert.Equal(2, new HashSet<T> { value, different }.Count);
    }
}
