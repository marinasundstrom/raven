namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class DiscriminatedUnionFactsTests
{
    [Fact]
    public void TryGetLogicalCaseNameFromMetadata_SupportsSeparatedAndLegacyFormats()
    {
        Assert.True(DiscriminatedUnionFacts.TryGetLogicalCaseNameFromMetadata("Result", "Result_Ok", out var separated));
        Assert.Equal("Ok", separated);

        Assert.True(DiscriminatedUnionFacts.TryGetLogicalCaseNameFromMetadata("Result", "ResultError", out var legacy));
        Assert.Equal("Error", legacy);

        Assert.False(DiscriminatedUnionFacts.TryGetLogicalCaseNameFromMetadata("Result", "Outcome_Ok", out var passthrough));
        Assert.Equal("Outcome_Ok", passthrough);
    }
}
