using Raven.CodeAnalysis;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TargetFrameworkMonikerTests
{
    [Fact]
    public void ToFrameworkString_converts_net_tfm()
    {
        var full = TargetFrameworkMoniker.Parse("net9.0").ToFrameworkString();
        Assert.Equal(".NETCoreApp,Version=v9.0", full);
    }

    [Fact]
    public void ToTfm_converts_full_string()
    {
        var tfm = TargetFrameworkMoniker.Parse(".NETCoreApp,Version=v9.0").ToTfm();
        Assert.Equal("net9.0", tfm);
    }

    [Fact]
    public void GetVersion_defaults_to_installed()
    {
        var tfm = TargetFrameworkResolver.GetVersion();
        Assert.False(string.IsNullOrWhiteSpace(tfm.ToFrameworkString()));
    }
}

