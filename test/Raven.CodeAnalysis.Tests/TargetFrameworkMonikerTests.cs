using Raven.CodeAnalysis;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TargetFrameworkMonikerTests
{
    [Fact]
    public void ToFrameworkString_converts_net_tfm()
    {
        var full = TargetFrameworkMoniker.ToFrameworkString("net9.0");
        Assert.Equal(".NETCoreApp,Version=v9.0", full);
    }

    [Fact]
    public void ToTfm_converts_full_string()
    {
        var tfm = TargetFrameworkMoniker.ToTfm(".NETCoreApp,Version=v9.0");
        Assert.Equal("net9.0", tfm);
    }

    [Fact]
    public void Resolve_defaults_to_installed()
    {
        var full = TargetFrameworkMoniker.Resolve();
        Assert.False(string.IsNullOrWhiteSpace(full));
    }
}

