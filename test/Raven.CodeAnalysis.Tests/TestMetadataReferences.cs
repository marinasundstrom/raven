using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;
using Raven.MetadataFixtures.Linq;

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

    private static readonly Lazy<MetadataReference> s_extensionMethodsFixture = new(() =>
        MetadataReference.CreateFromFile(typeof(RavenEnumerableExtensions).Assembly.Location));

    public static string TargetFramework => s_default.Value.tfm;
    public static MetadataReference[] Default => s_default.Value.refs;
    public static MetadataReference ExtensionMethodsFixture => s_extensionMethodsFixture.Value;
    public static MetadataReference[] DefaultWithExtensionMethods
        => [.. Default, ExtensionMethodsFixture];
}
