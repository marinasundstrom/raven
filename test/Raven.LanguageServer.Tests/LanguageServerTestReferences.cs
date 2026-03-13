using System.IO;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.Editor.Tests;

internal static class LanguageServerTestReferences
{
    private const string TargetFramework = "net10.0";

    public static MetadataReference[] Default { get; } = TargetFrameworkResolver
        .GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(TargetFramework))
        .Where(File.Exists)
        .Select(MetadataReference.CreateFromFile)
        .ToArray();
}
