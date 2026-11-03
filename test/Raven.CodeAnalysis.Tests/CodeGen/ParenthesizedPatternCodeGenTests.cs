using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ParenthesizedPatternCodeGenTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Grouping_PreservesPatternOperations(bool warmDiagnostics)
    {
        var tree = SyntaxTree.ParseText("class Probe { static func Match(value: int) -> bool => value is not (1 or 2) }");
        var compilation = Compilation.Create("GroupedPatternOperations", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        if (warmDiagnostics)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var grouped = tree.GetRoot().DescendantNodes().OfType<ParenthesizedPatternSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IOrPatternOperation>(model.GetOperation(grouped));
        Assert.NotNull(operation.Left);
        Assert.NotNull(operation.Right);
        var negated = Assert.IsAssignableFrom<INotPatternOperation>(model.GetOperation(grouped.Parent!));
        Assert.IsAssignableFrom<IOrPatternOperation>(negated.Pattern);
    }

    [Theory]
    [InlineData(1, false)]
    [InlineData(2, false)]
    [InlineData(3, true)]
    public void NotOfGroupedAlternatives_MatchesScalar(int value, bool expected)
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Match(value: int) -> bool => value is not (1 or 2)
            }
            """);
        var compilation = Compilation.Create("GroupedPattern" + Guid.NewGuid().ToString("N"), [tree],
            TestMetadataReferences.Default, CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Match")!;
        Assert.Equal(expected, method.Invoke(null, [value]));
    }
    [Fact]
    public void SingletonPositionalPattern_MatchesTupleRatherThanScalar()
    {
        var tree = SyntaxTree.ParseText("class Probe { static func Match(value: object) -> bool => value is (1,) }");
        var compilation = Compilation.Create("SingletonPattern", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var method = loaded.Assembly.GetType("Probe")!.GetMethod("Match")!;
        Assert.Equal(true, method.Invoke(null, [ValueTuple.Create(1)]));
        Assert.Equal(false, method.Invoke(null, [ValueTuple.Create(2)]));
        Assert.Equal(false, method.Invoke(null, [1]));
    }

}
