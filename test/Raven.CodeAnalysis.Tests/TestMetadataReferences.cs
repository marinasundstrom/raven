using System;
using System.IO;
using Raven.CodeAnalysis;
using System.Linq;

namespace Raven.CodeAnalysis.Tests;

internal static class TestMetadataReferences
{
    private static readonly Lazy<(string tfm, MetadataReference[] refs)> s_default = new(() =>
    {
        var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        var refs = TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Where(File.Exists)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
        return (version.Moniker.ToTfm(), refs);
    });

    public static string TargetFramework => s_default.Value.tfm;
    public static MetadataReference[] Default => s_default.Value.refs;
}

