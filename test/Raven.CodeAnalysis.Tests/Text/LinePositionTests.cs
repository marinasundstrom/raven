using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Text;

public sealed class LinePositionTests
{
    [Fact]
    public void EqualsObject_UsesValueSemanticsAndAcceptsNull()
    {
        var position = new LinePosition(2, 3);

        Assert.True(position.Equals((object)new LinePosition(2, 3)));
        Assert.False(position.Equals((object)new LinePosition(2, 4)));
        Assert.False(position.Equals(null));
    }
}
