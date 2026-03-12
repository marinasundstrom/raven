using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Bugs;

public class MetadataReferenceLoadingTests
{
    [Fact]
    public void GetTypeByMetadataName_LoadsReferences_WhenNoSyntaxTrees()
    {
        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var referencePaths = TargetFrameworkResolver.GetReferenceAssemblies(version);
        var references = referencePaths.Select(MetadataReference.CreateFromFile).ToArray();

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddReferences(references);

        var consoleType = compilation.GetTypeByMetadataName("System.Console");
        Assert.NotNull(consoleType);

        var stringType = compilation.GetTypeByMetadataName("System.String");
        Assert.NotNull(stringType);
    }

    [Fact]
    public void WildcardImport_ResolvesNestedMetadataNamespace()
    {
        var metadataReference = TestMetadataFactory.CreateFileReferenceFromSource(
            """
namespace Outer.Inner {
    public static class Observer {
        public static func Ping() -> int {
            return 42
        }
    }
}
""",
            assemblyName: "nested-metadata-fixture");

        var tree = SyntaxTree.ParseText(
            """
import Outer.Inner.*

val value = Observer.Ping()
""");

        var compilation = Compilation.Create(
            "consumer",
            [tree],
            [.. TestMetadataReferences.Default, metadataReference],
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, static d => d.Severity == DiagnosticSeverity.Error);
    }
}
