using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ImportedGenericSignatureExecutionTests
{
    [Theory]
    [InlineData("Wide().Rest.Item1", 8)]
    [InlineData("GenericWide<int>(42).Rest.Item1", 42)]
    [InlineData("GenericArray<int>(0)[0]", 12)]
    public void ImportedSignatureResolvesAtRuntime(string expression, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            import Raven.CodeAnalysis.Tests.ImportedSignatureFixture.*
            class Probe { static func Read() -> int => {{expression}} }
            """);
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(ImportedSignatureFixture).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("ImportedSignatureConsumer", [tree], references, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
        Assert.Empty(errors);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, references);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Read")!.Invoke(null, null));
    }
}

public static class ImportedSignatureFixture
{
    public static (int, int, int, int, int, int, int, int) Wide() => (1, 2, 3, 4, 5, 6, 7, 8);

    public static (int, int, int, int, int, int, int, T) GenericWide<T>(T last) => (1, 2, 3, 4, 5, 6, 7, last);

    public static int[] GenericArray<T>(T ignored) => [12];
}
