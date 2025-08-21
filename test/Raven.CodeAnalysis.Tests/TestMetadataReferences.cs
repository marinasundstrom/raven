using System;
using System.IO;
using Raven.CodeAnalysis;
using System.Linq;

namespace Raven.CodeAnalysis.Tests;

internal static class TestMetadataReferences
{
    private static readonly Lazy<MetadataReference[]> s_default = new(() =>
    {
        var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        return TargetFrameworkResolver.GetReferenceAssemblies(version)
            .Where(File.Exists)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
    });

    public static MetadataReference[] Default => s_default.Value;
}

