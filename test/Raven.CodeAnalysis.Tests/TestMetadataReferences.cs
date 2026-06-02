using System;
using System.IO;
using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

internal static class TestMetadataReferences
{
    private static readonly Lazy<(string tfm, MetadataReference[] refs)> s_default = new(() =>
    {
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refs = TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Where(File.Exists)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
        return (TestTargetFramework.Default, refs);
    });

    private static readonly Lazy<MetadataReference[]> s_defaultWithoutSystemLinq = new(() =>
        Default.Where(reference => !IsSystemLinq(reference)).ToArray());

    private static readonly Lazy<MetadataReference[]> s_defaultWithoutSystemLinqWithExtensionMethods = new(() =>
        [.. DefaultWithoutSystemLinq, ExtensionMethodsFixture]);

    private static readonly Lazy<MetadataReference> s_extensionMethodsFixture = new(() =>
        MetadataReference.CreateFromFile(typeof(Raven.MetadataFixtures.Linq.RavenEnumerableExtensions).Assembly.Location));

    private static readonly Lazy<MetadataReference> s_ravenCore = new(() =>
    {
        var path = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (!File.Exists(path))
        {
            throw new FileNotFoundException(
                "Raven.Core.dll was not found in the test output directory. Build Raven.CodeAnalysis.Tests so the Raven.Core project output is copied before using TestMetadataReferences.RavenCore.",
                path);
        }

        return MetadataReference.CreateFromFile(path);
    });

    private static readonly Lazy<MetadataReference[]> s_defaultWithRavenCore = new(() =>
        [.. Default, RavenCore]);

    public static string TargetFramework => s_default.Value.tfm;
    public static MetadataReference[] Default => s_default.Value.refs;
    public static MetadataReference ExtensionMethodsFixture => s_extensionMethodsFixture.Value;
    public static MetadataReference RavenCore => s_ravenCore.Value;
    public static MetadataReference[] DefaultWithoutSystemLinq => s_defaultWithoutSystemLinq.Value;
    public static MetadataReference[] DefaultWithoutSystemLinqWithExtensionMethods => s_defaultWithoutSystemLinqWithExtensionMethods.Value;
    public static MetadataReference[] DefaultWithExtensionMethods
        => [.. Default, ExtensionMethodsFixture];
    public static MetadataReference[] DefaultWithRavenCore => s_defaultWithRavenCore.Value;

    private static bool IsSystemLinq(MetadataReference reference)
    {
        if (reference is not PortableExecutableReference portable)
            return false;

        var path = portable.FilePath;
        if (string.IsNullOrEmpty(path))
            return false;

        var fileName = Path.GetFileName(path);
        return string.Equals(fileName, "System.Linq.dll", StringComparison.OrdinalIgnoreCase);
    }
}
