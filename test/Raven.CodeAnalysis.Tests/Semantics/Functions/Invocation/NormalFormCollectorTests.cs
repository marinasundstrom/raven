using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NormalFormCollectorTests
{
    [Fact, Trait("Target", "DotNet")]
    public void DotNet_NormalArrayFormMatchesCSharp() => Check(TargetPlatform.DotNet);

    [Fact, Trait("Target", "NeoCLR")]
    public void NeoCLR_NormalArrayFormMatchesCSharp() => Check(TargetPlatform.NeoCLR);

    [Theory, Trait("Target", "DotNet")]
    [InlineData("Pick(values)", 2)]
    [InlineData("Pick(items: values)", 2)]
    [InlineData("Pick(7)", 1)]
    [InlineData("Pick(7, 9)", 2)]
    public void DotNet_GenericCollectorChoosesNormalOrExpandedForm(string call, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static func Pick<T>(item: T) -> int => 1
                static func Pick<T>(params items: T[]) -> int => 2
                static func Run(values: int[]) -> int => {{call}}
            }
            """);
        var compilation = Compilation.Create("Collector", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var run = loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!;
        Assert.Equal(expected, run.Invoke(null, [new[] { 1, 2 }]));
    }

    private static void Check(TargetPlatform target)
    {
        byte[] bytes = [3, 5];
        System.Collections.Immutable.ImmutableArray<byte> expected = System.Collections.Immutable.ImmutableArray.Create(bytes);
        Assert.Equal(bytes, expected.ToArray());

        var tree = SyntaxTree.ParseText("""
            import System.Collections.Immutable.*
            class Probe {
                static func Copy(values: byte[]) -> ImmutableArray<byte> => ImmutableArray.Create(values)
            }
            """);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target);
        var compilation = Compilation.Create("NormalArray", [tree], TestMetadataReferences.Default, options);
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var call = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(call).Symbol);
        Assert.True(Assert.Single(method.Parameters).IsVarParams);
        Assert.Equal(SpecialType.System_Byte, Assert.Single(method.TypeArguments).SpecialType);

        if (target == TargetPlatform.DotNet)
        {
            using var image = new MemoryStream();
            var emitted = compilation.Emit(image);
            Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
            using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
            var copy = loaded.Assembly.GetType("Probe", true)!.GetMethod("Copy")!;
            var actual = Assert.IsType<System.Collections.Immutable.ImmutableArray<byte>>(copy.Invoke(null, [bytes]));
            Assert.Equal(expected.ToArray(), actual.ToArray());
        }
    }
}
