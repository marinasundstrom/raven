using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed partial class MacroInputSnapshotTests
{
    [Theory]
    [InlineData(9, false)]
    [InlineData(10, true)]
    [InlineData(11, true)]
    [InlineData(12, false)]
    [InlineData(20, false)]
    [InlineData(21, false)]
    public void TokenLookup_UsesHalfOpenSpanAndBodyBounds(int position, bool found)
    {
        var token = Token(new TextSpan(10, 2));
        var snapshot = new MacroInputSnapshot(new TextSpan(10, 10), [token], []);
        Assert.Same(found ? token : null, snapshot.FindToken(position));
    }

    [Fact]
    public void TokenLookup_AcceptsZeroWidthAtBodyEndAndRetainsOrder()
    {
        var first = Token(new TextSpan(20, 0));
        var second = Token(new TextSpan(20, 0));
        var snapshot = new MacroInputSnapshot(new TextSpan(10, 10), [first, second], []);
        Assert.Same(first, snapshot.FindToken(20));
        Assert.Null(snapshot.FindToken(19));
    }

    [Theory]
    [InlineData(9, false)]
    [InlineData(10, true)]
    [InlineData(20, true)]
    [InlineData(21, false)]
    public void FragmentLookup_IncludesRegionEndWithinBody(int position, bool found)
    {
        var region = Region(new TextSpan(10, 10));
        var snapshot = new MacroInputSnapshot(new TextSpan(10, 10), [], [region]);
        Assert.Same(found ? region : null, snapshot.FindFragmentRegion(position));
    }

    [Fact]
    public void FragmentLookup_PrefersSmallestThenFirstAndSupportsEmptyRegions()
    {
        var outer = Region(new TextSpan(10, 10));
        var inner = Region(new TextSpan(12, 2));
        var tied = Region(new TextSpan(12, 2));
        var empty = Region(new TextSpan(13, 0));
        var snapshot = new MacroInputSnapshot(new TextSpan(10, 10), [], [outer, inner, tied, empty]);
        Assert.Same(inner, snapshot.FindFragmentRegion(12));
        Assert.Same(empty, snapshot.FindFragmentRegion(13));
        Assert.Same(inner, snapshot.FindFragmentRegion(14));
        Assert.Same(outer, snapshot.FindFragmentRegion(15));
    }

    [Fact]
    public void Snapshot_IsAGetterOnlyValueOverItsStoredArrays()
    {
        ImmutableArray<MacroTokenInfo> tokens = [Token(new TextSpan(10, 1))];
        ImmutableArray<MacroFragmentRegion> regions = [Region(new TextSpan(10, 1))];
        var first = new MacroInputSnapshot(new TextSpan(10, 10), tokens, regions);
        var second = new MacroInputSnapshot(new TextSpan(10, 10), tokens, regions);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.NotEqual(first, new MacroInputSnapshot(new TextSpan(10, 11), tokens, regions));
        Assert.Null(typeof(MacroInputSnapshot).GetProperty("Tokens")!.SetMethod);
    }

    private static MacroTokenInfo Token(TextSpan span)
        => new(SyntaxFactory.Identifier("x"), span, span, "identifier", MacroTokenClassification.Identifier, null);

    private static MacroFragmentRegion Region(TextSpan span)
        => new(MacroFragmentKind.Expression, span, span, [], null);
}
