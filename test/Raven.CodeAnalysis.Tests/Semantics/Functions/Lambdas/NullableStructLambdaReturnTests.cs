using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NullableStructLambdaReturnTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData(false, false, false)]
    [InlineData(false, false, true)]
    [InlineData(false, true, false)]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, false, true)]
    [InlineData(true, true, false)]
    [InlineData(true, true, true)]
    public void ConditionalLambdaUsesNullableStructDelegateReturn(bool generic, bool parenthesized, bool customStruct)
    {
        var declaration = generic
            ? "func Apply<T>(value: T, mapper: System.Func<int, int?>) -> int? { return mapper(0) }"
            : "func Apply(value: int, mapper: System.Func<int, int?>) -> int? { return mapper(0) }";
        var expression = "if " + (customStruct ? "value.Size" : "value") + " == 0 { null } else { value }";
        if (parenthesized)
            expression = "(" + expression + ")";
        if (customStruct)
            declaration = "record struct Value(Size: int)\n" + declaration.Replace("System.Func<int, int?>", "System.Func<Value, Value?>").Replace("-> int?", "-> Value?").Replace("mapper(0)", "mapper(Value(0))");
        var tree = SyntaxTree.ParseText(declaration + "\nlet result = Apply(1, func(value) => " + expression + ")");
        var compilation = Compilation.Create("NullableLambda", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrPreservesNullableReturnThroughParentheses()
    {
        var tree = SyntaxTree.ParseText("func Apply(mapper: System.Func<int, int?>) -> int? { return mapper(0) }\nlet result = Apply(func(value) => (if value == 0 { null } else { value }))");
        var compilation = Compilation.Create("NeoNullableLambda", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithTargetPlatform(TargetPlatform.NeoCLR));
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void ParenthesizedNullableLambdaExecutesBothBranches()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Run() -> bool {
                    let mapper: System.Func<int, int?> = func(value) => (if value == 0 { null } else { value })
                    return mapper(0) is null && mapper(2) == 2
                }
            }
            """);
        var compilation = Compilation.Create("NullableLambdaRuntime", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(true, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

}
