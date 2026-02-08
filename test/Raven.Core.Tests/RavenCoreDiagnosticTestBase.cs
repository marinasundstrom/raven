using System.Collections.Immutable;
using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Testing;

namespace Raven.Core.Tests;

public abstract class RavenCoreDiagnosticTestBase
{
    protected DiagnosticVerifier CreateVerifier(
        string testCode,
        IEnumerable<DiagnosticResult>? expectedDiagnostics = null,
        IEnumerable<string>? disabledDiagnostics = null)
    {
        var disabled = disabledDiagnostics?.ToList() ?? [];
        disabled.Add("RAV1011");
        disabled.Add("RAV1014");

        var ravenCorePath = ResolveRavenCorePath();
        var ravenCoreReference = MetadataReference.CreateFromFile(ravenCorePath);

        return new DiagnosticVerifier
        {
            Test = new Test
            {
                TestCode = testCode,
                ExpectedDiagnostics = expectedDiagnostics?.ToList() ?? [],
                DisabledDiagnostics = disabled,
                State = new TestState
                {
                    AdditionalReferences = ImmutableArray.Create<MetadataReference>(ravenCoreReference),
                    ReferenceAssemblies = ReferenceAssemblies.Default,
                }
            }
        };
    }

    private static string ResolveRavenCorePath()
    {
        var outputPath = Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll");
        if (File.Exists(outputPath))
        {
            return outputPath;
        }

        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var fallbackPath = Path.Combine(repoRoot, "src", "Raven.Core", "bin", "Debug", "net9.0", "Raven.Core.dll");
        if (File.Exists(fallbackPath))
        {
            return fallbackPath;
        }

        throw new FileNotFoundException("Could not locate Raven.Core.dll for tests.", outputPath);
    }
}
