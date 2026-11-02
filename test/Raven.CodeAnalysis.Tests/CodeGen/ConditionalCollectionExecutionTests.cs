using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalCollectionExecutionTests
{
    [Fact]
    public void InferredEmptyBranch_PreservesBranchEvaluationAndArrayIdentity()
    {
        var tree = SyntaxTree.ParseText("""
class Probe {
    var Calls: int
    func Values(values: int[]) -> int[] { Calls++; return values }
    func Pick(flag: bool, values: int[]) -> int[] {
        let result = if flag { [] } else { Values(values) }
        return result
    }
    func Reverse(flag: bool, values: int[]) -> int[] {
        let result = if flag { Values(values) } else { [] }
        return result
    }
}
""");
        var compilation = Compilation.Create("ConditionalCollectionExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Probe")!;
        var instance = Activator.CreateInstance(type);
        var values = new[] { 42 };
        Assert.Empty(Assert.IsType<int[]>(type.GetMethod("Pick")!.Invoke(instance, [true, values])));
        Assert.Empty(Assert.IsType<int[]>(type.GetMethod("Reverse")!.Invoke(instance, [false, values])));
        Assert.Equal(0, type.GetProperty("Calls")!.GetValue(instance));
        Assert.Same(values, type.GetMethod("Pick")!.Invoke(instance, [false, values]));
        Assert.Same(values, type.GetMethod("Reverse")!.Invoke(instance, [true, values]));
        Assert.Equal(2, type.GetProperty("Calls")!.GetValue(instance));
    }
}
