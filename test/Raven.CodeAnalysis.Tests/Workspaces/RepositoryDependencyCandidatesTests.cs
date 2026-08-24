using Raven;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class RepositoryDependencyCandidatesTests
{
    [Fact]
    public void Create_UsesRequestedConfigurationForRepositoryFallbacks()
    {
        var candidates = RepositoryDependencyCandidates.Create(
            Path.Combine(Path.DirectorySeparatorChar.ToString(), "tool", "bin"),
            Path.Combine(Path.DirectorySeparatorChar.ToString(), "repo"),
            "Raven.Macros",
            "Release",
            "net10.0",
            ["net11.0"],
            "Raven.Macros.dll",
            "Raven.Macros.dll",
            "../../sdk/Raven.Macros.dll");

        Assert.Contains(
            Path.Combine(Path.DirectorySeparatorChar.ToString(), "repo", "src", "Raven.Macros", "bin", "Release", "net10.0", "Raven.Macros.dll"),
            candidates);
        Assert.Contains(
            Path.Combine(Path.DirectorySeparatorChar.ToString(), "repo", "src", "Raven.Macros", "bin", "Release", "net11.0", "net11.0", "Raven.Macros.dll"),
            candidates);
        Assert.DoesNotContain(candidates, path => path.Contains($"{Path.DirectorySeparatorChar}Debug{Path.DirectorySeparatorChar}", StringComparison.Ordinal));
    }
}
