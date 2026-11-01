using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class SeekableTextSourceTests
{
    [Theory]
    [InlineData(2)]
    [InlineData(int.MaxValue)]
    public void SequentialReads_PreserveCharactersAndEndPosition(int bufferSize)
    {
        const string text = "a🙂zxyz";
        var source = new SeekableTextSource(new StringReader(text), maxBufferSize: bufferSize);
        foreach (var ch in text)
        {
            Assert.Equal(ch, source.Peek());
            Assert.Equal(ch, source.Read());
        }
        Assert.Equal(text.Length, source.Position);
        Assert.Equal(-1, source.Read());
        Assert.Equal(-1, source.Peek());
        Assert.Equal(text.Length, source.Position);
        Assert.False(source.PeekChar(0, out var end));
        Assert.Equal('\0', end);
    }

    [Theory]
    [InlineData(2)]
    [InlineData(int.MaxValue)]
    public void Lookahead_DoesNotDiscardUnreadCharacters(int bufferSize)
    {
        var source = new SeekableTextSource(new StringReader("abcdef"), maxBufferSize: bufferSize);
        Assert.True(source.PeekChar(4, out var future));
        Assert.Equal('e', future);
        Assert.Equal(0, source.Position);
        Assert.Equal('a', source.Read());
        Assert.False(source.PeekChar(20, out var end));
        Assert.Equal('\0', end);
        Assert.Equal('b', source.Read());
    }

    [Theory]
    [InlineData(2)]
    [InlineData(int.MaxValue)]
    public void SavedPositions_PreserveRewindAcrossBufferPressure(int bufferSize)
    {
        var source = new SeekableTextSource(new StringReader("abcdef"), maxBufferSize: bufferSize);
        Assert.Equal(0, source.PushPosition());
        Assert.Equal('a', source.Read());
        Assert.Equal(1, source.PushPosition());
        Assert.Equal('b', source.Read());
        Assert.Equal('c', source.Read());
        Assert.Equal('d', source.Read());
        Assert.Equal(1, source.PopAndRestorePosition());
        Assert.Equal('b', source.Read());
        Assert.Equal(0, source.PopAndRestorePosition());
        Assert.Equal('a', source.Read());
    }

    [Fact]
    public void ResetPosition_UsesAbsoluteOffsetAndClampsToAvailableInput()
    {
        var source = new SeekableTextSource(new StringReader("abcd"), initialPosition: 10);
        Assert.Equal(10, source.Position);
        source.ResetPosition(12);
        Assert.Equal('c', source.Read());
        source.ResetPosition(-1);
        Assert.Equal(10, source.Position);
        Assert.Equal('a', source.Read());
        source.ResetPosition(100);
        Assert.Equal(14, source.Position);
        Assert.Equal(-1, source.Read());
    }

    [Fact]
    public void ReleasedCheckpoint_AllowsTrimmingConsumedHistory()
    {
        var source = new SeekableTextSource(new StringReader("abcdef"), maxBufferSize: 2);
        source.PushPosition();
        for (var i = 0; i < 4; i++)
            Assert.Equal("abcd"[i], source.Read());
        Assert.Equal(0, source.PopPosition());
        Assert.Equal(4, source.Position);
        source.ResetPosition(0);
        Assert.Equal(2, source.Position);
        Assert.Equal('c', source.Read());
    }

    [Fact]
    public void Constructor_RejectsInvalidReaderCoordinatesAndLimit()
    {
        Assert.Throws<ArgumentNullException>(() => new SeekableTextSource(null!));
        Assert.Throws<ArgumentOutOfRangeException>(() => new SeekableTextSource(new StringReader(""), -1));
        Assert.Throws<ArgumentOutOfRangeException>(() => new SeekableTextSource(new StringReader(""), maxBufferSize: 0));
        Assert.Throws<ArgumentOutOfRangeException>(() => new SeekableTextSource(new StringReader(""), maxBufferSize: -1));
    }

}
