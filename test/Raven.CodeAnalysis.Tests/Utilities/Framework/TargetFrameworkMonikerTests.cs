using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TargetFrameworkMonikerTests
{
    [Fact]
    public void ToFrameworkString_converts_net_tfm()
    {
        var full = TargetFrameworkMoniker.Parse("net10.0").ToFrameworkString();
        Assert.Equal(".NETCoreApp,Version=v10.0", full);
    }

    [Fact]
    public void ToTfm_converts_full_string()
    {
        var tfm = TargetFrameworkMoniker.Parse(".NETCoreApp,Version=v10.0").ToTfm();
        Assert.Equal("net10.0", tfm);
    }

    [Fact]
    public void NanoFramework_round_trips_tfm_and_full_name()
    {
        var tfm = TargetFrameworkMoniker.Parse("netnano1.0");

        Assert.Equal(FrameworkId.NetNanoFramework, tfm.Framework);
        Assert.Equal(".NETnanoFramework,Version=v1.0", tfm.ToFrameworkString());
        Assert.Equal("netnano1.0", TargetFrameworkMoniker.Parse(tfm.ToFrameworkString()).ToTfm());
        Assert.Equal(".NET nanoFramework 1.0", tfm.GetDisplayName());
    }

    [Fact]
    public void ResolveVersion_defaults_to_installed()
    {
        var version = TargetFrameworkResolver.ResolveVersion();
        Assert.False(string.IsNullOrWhiteSpace(version.ToFrameworkString()));
    }
}
