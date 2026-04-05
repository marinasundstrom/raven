namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class UnionFactsTests
{
    [Fact]
    public void TryGetLogicalCaseNameFromMetadata_SupportsSeparatedAndLegacyFormats()
    {
        Assert.True(UnionFacts.TryGetLogicalCaseNameFromMetadata("Result", "Result_Ok", out var separated));
        Assert.Equal("Ok", separated);

        Assert.True(UnionFacts.TryGetLogicalCaseNameFromMetadata("Result", "ResultError", out var legacy));
        Assert.Equal("Error", legacy);

        Assert.False(UnionFacts.TryGetLogicalCaseNameFromMetadata("Result", "Outcome_Ok", out var passthrough));
        Assert.Equal("Outcome_Ok", passthrough);
    }
}
