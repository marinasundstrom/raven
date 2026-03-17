using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Syntax;

public sealed class TextSpanTests
{
    [Fact]
    public void FromBounds_CreatesSpanFromStartAndEnd()
    {
        var span = TextSpan.FromBounds(3, 8);

        span.Start.ShouldBe(3);
        span.Length.ShouldBe(5);
        span.End.ShouldBe(8);
    }

    [Fact]
    public void FromBounds_ThrowsWhenEndPrecedesStart()
    {
        Should.Throw<ArgumentOutOfRangeException>(() => TextSpan.FromBounds(8, 3));
    }
}
