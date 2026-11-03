using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class ResolvedAssemblyArtifactValueTests
{
    [Fact]
    public void ArtifactIdentityUsesAllValuesAndSupportsImmutableCopies()
    {
        var original = new ResolvedAssemblyArtifact("Example", new Version(1, 2), "en", "abcd", 1, "sha256");
        var equivalent = new ResolvedAssemblyArtifact("Example", new Version(1, 2), "en", "abcd", 1, "sha256");
        Assert.NotSame(original, equivalent);
        Assert.Equal(original, equivalent);
        Assert.True(original == equivalent);
        Assert.Equal(original.GetHashCode(), equivalent.GetHashCode());
        Assert.Contains(equivalent, new HashSet<ResolvedAssemblyArtifact> { original });
        foreach (var changed in new[]
        {
            original with { Name = "Other" },
            original with { Version = new Version(1, 3) },
            original with { Culture = "sv" },
            original with { PublicKeyToken = "ef01" },
            original with { Flags = 2 },
            original with { Sha256 = "different" },
        })
            Assert.True(original != changed);
        Assert.Equal("sha256", original.Sha256);
    }
}
