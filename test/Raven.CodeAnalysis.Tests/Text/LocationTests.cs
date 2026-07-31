using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Text;

public sealed class LocationTests
{
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
