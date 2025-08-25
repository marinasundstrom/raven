using Raven.CodeAnalysis;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TargetFrameworkResolverTests
{
    [Fact]
    public void ResolveLatestInstalledVersion_returns_installed_framework()
    {
        var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        Assert.False(string.IsNullOrWhiteSpace(version.ToFrameworkString()));
    }
}
