using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Text;

[Trait("Target", "Shared")]
public sealed class TextCoordinateContractTests
{
    [Theory]
    [InlineData(-1, 0)]
    [InlineData(0, -1)]
    public void TextSpan_RejectsNegativeCoordinates(int start, int length)
    {
        Assert.Throws<ArgumentOutOfRangeException>(() => new TextSpan(start, length));
    }

    [Theory]
    [InlineData(int.MaxValue, 1)]
    [InlineData(1, int.MaxValue)]
    [InlineData(int.MaxValue, int.MaxValue)]
    public void TextSpan_RejectsUnrepresentableEnd(int start, int length)
    {
        Assert.Throws<ArgumentOutOfRangeException>(() => new TextSpan(start, length));
    }

    [Fact]
    public void TextSpan_AcceptsLargestRepresentableEnd()
    {
        Assert.Equal(int.MaxValue, new TextSpan(int.MaxValue, 0).End);
        Assert.Equal(int.MaxValue, new TextSpan(0, int.MaxValue).End);
        Assert.Equal(int.MaxValue, TextSpan.FromBounds(1, int.MaxValue).End);
    }

    [Fact]
    public void TextSpan_DefaultAndEmptyBoundsAgree()
    {
        Assert.Equal(default, TextSpan.FromBounds(0, 0));
        Assert.Equal(new TextSpan(7, 0), TextSpan.FromBounds(7, 7));
        Assert.Throws<ArgumentOutOfRangeException>(() => TextSpan.FromBounds(-1, 0));
    }

    [Fact]
    public void TextSpan_EqualityWorksThroughTypedAndBoxedContracts()
    {
        var span = new TextSpan(2, 3);
        IEquatable<TextSpan> equatable = span;
        Assert.True(equatable.Equals(new TextSpan(2, 3)));
        Assert.True(span.Equals((object)new TextSpan(2, 3)));
        Assert.False(span.Equals(null));
        Assert.False(span.Equals("text"));
        Assert.True(span == new TextSpan(2, 3));
        Assert.True(span != new TextSpan(2, 4));
        Assert.Equal(span.GetHashCode(), new TextSpan(2, 3).GetHashCode());
        Assert.Equal("[2..5]", span.ToString());
    }

    [Theory]
    [InlineData(1, 9, 2, 0, -1)]
    [InlineData(2, 3, 2, 4, -1)]
    [InlineData(2, 4, 2, 3, 1)]
    [InlineData(2, 3, 2, 3, 0)]
    public void TextSpan_OrdersByStartThenEnd(int start, int length, int otherStart, int otherLength, int sign)
    {
        IComparable<TextSpan> span = new TextSpan(start, length);
        Assert.Equal(sign, Math.Sign(span.CompareTo(new TextSpan(otherStart, otherLength))));
    }

    [Fact]
    public void TextSpan_ContainsHalfOpenPositionsAndCompleteSpans()
    {
        var span = new TextSpan(2, 3);
        Assert.False(span.Contains(1));
        Assert.True(span.Contains(2));
        Assert.True(span.Contains(4));
        Assert.False(span.Contains(5));
        Assert.True(span.Contains(new TextSpan(3, 2)));
        Assert.True(span.Contains(new TextSpan(5, 0)));
        Assert.False(span.Contains(new TextSpan(1, 2)));
        Assert.False(new TextSpan(2, 0).Contains(2));
        Assert.True(span.IntersectsWith(new TextSpan(4, 3)));
        Assert.False(span.IntersectsWith(new TextSpan(5, 3)));
        Assert.True(TextSpanExtensions.Contains(span, 2));
    }

    [Theory]
    [InlineData(-1, 0, "line")]
    [InlineData(0, -1, "character")]
    public void LinePosition_RejectsNegativeCoordinates(int line, int character, string parameter)
    {
        var error = Assert.Throws<ArgumentOutOfRangeException>(() => new LinePosition(line, character));
        Assert.Equal(parameter, error.ParamName);
    }

    [Theory]
    [InlineData(1, 9, 2, 0, -1)]
    [InlineData(2, 3, 2, 4, -1)]
    [InlineData(2, 4, 2, 3, 1)]
    [InlineData(2, 3, 2, 3, 0)]
    public void LinePosition_OrderingAndEqualityAgree(int line, int character, int otherLine, int otherCharacter, int sign)
    {
        var left = new LinePosition(line, character);
        var right = new LinePosition(otherLine, otherCharacter);
        IComparable<LinePosition> comparable = left;
        IEquatable<LinePosition> equatable = left;
        Assert.Equal(sign, Math.Sign(comparable.CompareTo(right)));
        Assert.Equal(sign == 0, equatable.Equals(right));
        Assert.Equal(sign == 0, left == right);
        Assert.Equal(sign != 0, left != right);
        Assert.Equal(sign < 0, left < right);
        Assert.Equal(sign > 0, left > right);
        Assert.Equal(sign <= 0, left <= right);
        Assert.Equal(sign >= 0, left >= right);
    }

    [Fact]
    public void LinePosition_DefaultEqualityAndDisplayAgree()
    {
        Assert.Equal(default, LinePosition.Zero);
        var position = new LinePosition(2, 3);
        Assert.Equal(position.GetHashCode(), new LinePosition(2, 3).GetHashCode());
        Assert.False(position.Equals("text"));
        Assert.Equal("Line 2, Character 3", position.ToString());
    }

    [Fact]
    public void PositionSpans_PreserveTheirCoordinates()
    {
        var start = new LinePosition(2, 3);
        var end = new LinePosition(4, 5);
        var span = new LinePositionSpan(start, end);
        var fileSpan = new FileLinePositionSpan("example.rvn", start, end);
        Assert.Equal(start, span.Start);
        Assert.Equal(end, span.End);
        Assert.Equal("example.rvn", fileSpan.Path);
        Assert.Equal(start, fileSpan.StartLinePosition);
        Assert.Equal(end, fileSpan.EndLinePosition);
    }

    [Fact]
    public void LinePositionSpan_RecordProvidesEqualityDeconstructionAndCopy()
    {
        var start = new LinePosition(1, 2);
        var end = new LinePosition(3, 4);
        var span = new LinePositionSpan(start, end);
        var equal = new LinePositionSpan(start, end);
        Assert.Equal("LinePositionSpan { Start = Line 1, Character 2, End = Line 3, Character 4 }", span.ToString());
        IEquatable<LinePositionSpan> equatable = span;
        Assert.True(equatable.Equals(equal));
        Assert.True(span == equal);
        Assert.False(span != equal);
        Assert.True(span.Equals((object)equal));
        Assert.False(span.Equals(null));
        Assert.Equal(span.GetHashCode(), equal.GetHashCode());
        var (actualStart, actualEnd) = span;
        Assert.Equal(start, actualStart);
        Assert.Equal(end, actualEnd);
        var changed = span with { End = new LinePosition(5, 6) };
        Assert.True(span != changed);
        Assert.Equal(end, span.End);
        Assert.Equal(new LinePosition(5, 6), changed.End);
        Assert.Equal(start, changed.Start);
        Assert.True(typeof(LinePositionSpan).IsValueType);
        Assert.All(typeof(LinePositionSpan).GetProperties(), property =>
            Assert.Contains(typeof(System.Runtime.CompilerServices.IsExternalInit),
                property.SetMethod!.ReturnParameter.GetRequiredCustomModifiers()));
    }

    [Fact]
    public void Coordinates_ExposeReadOnlyPropertiesAndValueTypeContracts()
    {
        foreach (var type in new[] { typeof(TextSpan), typeof(LinePosition) })
        {
            Assert.True(type.IsValueType);
            Assert.All(type.GetProperties(), property => Assert.Null(property.SetMethod));
        }
    }
}
