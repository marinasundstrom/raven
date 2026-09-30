using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Tests;

public sealed class DotNetRuntimeAssemblyPathResolverTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), "raven-host-paths", Guid.NewGuid().ToString("N"));

    private string CreateFile(string relativePath)
    {
        var path = Path.Combine(_root, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, "path-selection fixture; not a managed assembly");
        return path;
    }

    private string? ResolveShared(string reference)
        => DotNetRuntimeAssemblyPathResolver.FindImplementation(reference, Path.Combine(_root, "shared"));

    [Fact]
    public void NuGetPrefersMatchingLibPath()
    {
        var reference = CreateFile("packages/example/1.0/ref/net10.0/Example.dll");
        var expected = CreateFile("packages/example/1.0/lib/net10.0/Example.dll");
        CreateFile("packages/example/1.0/lib/net9.0/Example.dll");
        Assert.Equal(expected, DotNetRuntimeAssemblyPathResolver.FindImplementation(reference));
    }

    [Fact]
    public void NuGetFallbackRetainsExistingDescendingPathOrder()
    {
        var reference = CreateFile("packages/example/1.0/ref/net11.0/Example.dll");
        CreateFile("packages/example/1.0/lib/net8.0/Example.dll");
        var expected = CreateFile("packages/example/1.0/lib/net9.0/Example.dll");
        Assert.Equal(expected, DotNetRuntimeAssemblyPathResolver.FindImplementation(reference));
    }

    [Fact]
    public void SdkPackUsesMatchingSharedFrameworkVersion()
    {
        var reference = CreateFile("dotnet/packs/Microsoft.NETCore.App.Ref/11.0.0/ref/net11.0/Example.dll");
        var expected = CreateFile("dotnet/shared/Microsoft.NETCore.App/11.0.0/Example.dll");
        CreateFile("dotnet/shared/Microsoft.NETCore.App/11.0.1/Example.dll");
        Assert.Equal(expected, ResolveShared(reference));
        File.Delete(expected);
        Assert.Null(ResolveShared(reference));
    }

    [Theory]
    [InlineData("microsoft.netcore.app.ref", "Microsoft.NETCore.App")]
    [InlineData("microsoft.aspnetcore.app.ref", "Microsoft.AspNetCore.App")]
    public void FrameworkPackagePrefersInstalledExactVersionOverPackageLib(string package, string framework)
    {
        var reference = CreateFile($"packages/{package}/11.0.0/ref/net11.0/Example.dll");
        CreateFile($"packages/{package}/11.0.0/lib/net11.0/Example.dll");
        var expected = CreateFile($"shared/{framework}/11.0.0/Example.dll");
        Assert.Equal(expected, ResolveShared(reference));
    }

    [Fact]
    public void FrameworkPackageFallbackPrefersStableNumericVersionWithinRequestedMajor()
    {
        var reference = CreateFile("packages/microsoft.netcore.app.ref/11.0.0/ref/net11.0/Example.dll");
        CreateFile("shared/Microsoft.NETCore.App/11.0.2/Example.dll");
        var expected = CreateFile("shared/Microsoft.NETCore.App/11.0.10/Example.dll");
        CreateFile("shared/Microsoft.NETCore.App/11.0.99-preview.1/Example.dll");
        CreateFile("shared/Microsoft.NETCore.App/12.0.0/Example.dll");
        CreateFile("shared/Microsoft.NETCore.App/11.0.20/Other.dll");
        Assert.Equal(expected, ResolveShared(reference));
    }

    [Fact]
    public void FrameworkPackageUsesPackageLibWhenNoMatchingHostImplementationExists()
    {
        var reference = CreateFile("packages/microsoft.netcore.app.ref/11.0.0/ref/net11.0/Example.dll");
        CreateFile("shared/Microsoft.NETCore.App/12.0.0/Example.dll");
        var expected = CreateFile("packages/microsoft.netcore.app.ref/11.0.0/lib/net11.0/Example.dll");
        Assert.Equal(expected, ResolveShared(reference));
    }

    [Fact]
    public void MissingCandidatesAndOrdinaryAssemblyPathsHaveNoMapping()
    {
        var reference = CreateFile("packages/example/1.0/ref/net11.0/Example.dll");
        Assert.Null(DotNetRuntimeAssemblyPathResolver.FindImplementation(reference));
        Assert.Null(DotNetRuntimeAssemblyPathResolver.FindImplementation(CreateFile("ordinary/Example.dll")));
        Assert.Null(DotNetRuntimeAssemblyPathResolver.FindImplementation(null));
        Assert.Null(DotNetRuntimeAssemblyPathResolver.FindImplementation(""));
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
