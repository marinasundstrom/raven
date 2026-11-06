using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "Shared")]
public sealed class NumericMatchCoverageTests
{
    [Fact]
    public void RemovingIntervalsProducesOrderedInclusiveMissingPatterns()
    {
        var coverage = new NumericMatchCoverage(new(-10, 10));
        coverage.Cover(new(-6, -4));
        coverage.Cover(new(2, 4));
        Assert.False(coverage.IsComplete);
        Assert.Equal<string>(["..=-7", "-3..=1", "5.."], coverage.GetMissingCases(false));
        coverage.Cover(new(-10, 10));
        Assert.True(coverage.IsComplete);
        Assert.Empty(coverage.GetMissingCases(false));
    }

    [Theory]
    [InlineData('\n', "'\\u000A'")]
    [InlineData('\'', "'\\u0027'")]
    [InlineData('\\', "'\\u005C'")]
    [InlineData('a', "'a'")]
    public void MissingCharacterIsQuotedAndEscaped(char ch, string expected)
    {
        var coverage = new NumericMatchCoverage(new(char.MinValue, char.MaxValue));
        coverage.Cover(new(char.MinValue, ch - 1));
        coverage.Cover(new(ch + 1, char.MaxValue));
        Assert.Equal(expected, Assert.Single(coverage.GetMissingCases(true)));
    }

    [Fact]
    public void ResultValueEqualityIncludesFlagsAndOrderedCaseText()
    {
        var first = new MatchExhaustivenessInfo(isExhaustive: false, ImmutableArray.Create("a", "b"), hasCatchAll: true);
        var second = new MatchExhaustivenessInfo(false, ImmutableArray.Create("a", "b"), true);
        Assert.True(first == second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.False(first.IsExhaustive);
        Assert.True(first.HasCatchAll);
        Assert.NotEqual(first, new(false, ["b", "a"], true));
        Assert.NotEqual(first, new(false, first.MissingCases, false));
        Assert.NotEqual(default(MatchExhaustivenessInfo), new MatchExhaustivenessInfo(false, [], false));
        Assert.True(new MatchExhaustivenessOptions(ignoreCatchAllPatterns: true).IgnoreCatchAllPatterns);
        Assert.False(default(MatchExhaustivenessOptions).IgnoreCatchAllPatterns);
    }
}
