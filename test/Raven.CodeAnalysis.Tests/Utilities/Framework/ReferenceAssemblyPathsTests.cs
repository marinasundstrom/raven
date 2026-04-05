using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class ReferenceAssemblyPathsTests
{
    [Fact]
    public void GetReferenceAssemblyDir_accepts_version_without_wildcard()
    {
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var shortVer = version.Moniker.Version.ToString();
        var dir = ReferenceAssemblyPaths.GetReferenceAssemblyDir(shortVer, version.Moniker.ToTfm());
        Assert.False(string.IsNullOrEmpty(dir));
    }
}
