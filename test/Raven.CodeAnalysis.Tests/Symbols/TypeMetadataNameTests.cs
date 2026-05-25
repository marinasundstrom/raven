using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public class TypeMetadataNameTests
{
    [Fact]
    public void ToFullyQualifiedMetadataName_IncludesGenericArity()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var actionDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Action`1")!;

        Assert.Equal("System.Action`1", actionDefinition.ToFullyQualifiedMetadataName());
    }

    [Fact]
    public void GetClrType_ResolvesConstructedGenericFromMetadata()
    {
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default);

        var actionDefinition = (INamedTypeSymbol)compilation.GetTypeByMetadataName("System.Action`1")!;
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var constructed = compilation.ConstructGenericType(actionDefinition, new ITypeSymbol[] { stringType });

        var clrType = constructed.GetClrType(compilation);

        Assert.Equal(typeof(Action<string>), clrType);
    }

    [Fact]
    public void ResolveRuntimeType_MapsNuGetSharedFrameworkReferenceAssembly()
    {
        var aspNetCoreReference = TryFindAspNetCoreReferenceAssembly();
        if (aspNetCoreReference is null)
            return;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(TestMetadataReferences.Default)
            .AddReferences(MetadataReference.CreateFromFile(aspNetCoreReference));

        var webApplication = Assert.IsAssignableFrom<PENamedTypeSymbol>(
            compilation.GetTypeByMetadataName("Microsoft.AspNetCore.Builder.WebApplication"));

        var runtimeType = compilation.ResolveRuntimeType(webApplication);

        Assert.NotNull(runtimeType);
        Assert.Equal("Microsoft.AspNetCore.Builder.WebApplication", runtimeType.FullName);
    }

    private static string? TryFindAspNetCoreReferenceAssembly()
    {
        var userProfile = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
        if (string.IsNullOrEmpty(userProfile))
            return null;

        var packageRoot = Path.Combine(userProfile, ".nuget", "packages", "microsoft.aspnetcore.app.ref");
        if (!Directory.Exists(packageRoot))
            return null;

        return Directory
            .EnumerateFiles(packageRoot, "Microsoft.AspNetCore.dll", SearchOption.AllDirectories)
            .OrderByDescending(static path => TryParsePackageVersion(path))
            .ThenByDescending(static path => path, StringComparer.OrdinalIgnoreCase)
            .FirstOrDefault(static path => path.Contains($"{Path.DirectorySeparatorChar}ref{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase));
    }

    private static Version? TryParsePackageVersion(string path)
    {
        var refIndex = path.IndexOf($"{Path.DirectorySeparatorChar}ref{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase);
        if (refIndex < 0)
            return null;

        var packageVersionDirectory = path[..refIndex];
        var version = Path.GetFileName(packageVersionDirectory);
        if (string.IsNullOrWhiteSpace(version))
            return null;

        var prereleaseIndex = version.IndexOf('-');
        var versionText = prereleaseIndex >= 0 ? version[..prereleaseIndex] : version;
        return Version.TryParse(versionText, out var parsed) ? parsed : null;
    }
}
