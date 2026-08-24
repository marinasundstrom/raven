using Raven;
using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class IlVerifyRunnerTests
{
    [Fact]
    public void GetReferencePaths_IncludesTargetRuntimeImplementationDependencies()
    {
        var framework = TargetFrameworkResolver.ResolveVersion("net10.0");
        var references = TargetFrameworkResolver
            .GetReferenceAssemblies(framework)
            .Select(MetadataReference.CreateFromFile)
            .ToArray();
        var compilation = Compilation.Create(
            "ilverify-target-runtime",
            [],
            references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var runtimeDirectory = IlVerifyRunner.ResolveRuntimeDirectory(compilation);
        var referencePaths = IlVerifyRunner.GetReferencePaths(compilation).ToArray();

        Assert.NotNull(runtimeDirectory);
        Assert.StartsWith("10.", Path.GetFileName(runtimeDirectory));
        Assert.Contains(
            Path.Combine(runtimeDirectory!, "System.Private.Xml.Linq.dll"),
            referencePaths);
        Assert.DoesNotContain(
            referencePaths,
            path => path.Contains($"{Path.DirectorySeparatorChar}Microsoft.NETCore.App{Path.DirectorySeparatorChar}11.", StringComparison.Ordinal));
    }
}
