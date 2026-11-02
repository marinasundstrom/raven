using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Text;

public sealed class LocationTests
{
    [Fact]
    [Trait("Target", "Shared")]
    public void ExternalLocation_PreservesSpanAndFileIdentity()
    {
        var span = new TextSpan(4, 5);
        var lines = new LinePositionSpan(new LinePosition(1, 2), new LinePosition(1, 7));
        var location = Location.Create("first.rvn", span, lines);
        var equal = Location.Create("first.rvn", span, lines);
        Assert.Equal(span, location.SourceSpan);
        Assert.Equal(new FileLinePositionSpan("first.rvn", lines.Start, lines.End), location.GetLineSpan());
        Assert.True(location.IsInSource);
        Assert.Null(location.SourceTree);
        Assert.Equal(location, equal);
        Assert.True(location == equal);
        Assert.Equal(location.GetHashCode(), equal.GetHashCode());
        Assert.NotEqual(location, Location.Create("second.rvn", span, lines));
        Assert.NotEqual(location, Location.Create("first.rvn", new TextSpan(4, 6), lines));
        Assert.NotEqual(location, Location.Create("first.rvn", span,
            new LinePositionSpan(new LinePosition(2, 2), new LinePosition(2, 7))));
    }

    [Fact]
    public void LocationShape_ExposesOnlyTheApplicableBackingDocument()
    {
        Assert.Null(Location.None.SourceTree);
        Assert.Null(Location.None.MetadataModule);

        var tree = SyntaxTree.ParseText("let value = 1");
        var location = Location.Create(tree, new TextSpan(4, 5));

        Assert.Same(tree, location.SourceTree);
        Assert.Null(location.MetadataModule);
    }
}
