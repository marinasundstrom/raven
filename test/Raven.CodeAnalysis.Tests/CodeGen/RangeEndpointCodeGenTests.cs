using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RangeEndpointCodeGenTests
{
    [Theory]
    [InlineData("1..3", 250)]
    [InlineData("1..=2", 250)]
    [InlineData("^3..=^2", 250)]
    [InlineData("..=^1", 450)]
    [InlineData("..=0", 120)]
    public void StoredRangeAndSlice_PreserveEndpointMeaning(string range, int expected)
    {
        var source = $$"""
class Probe {
    public static func Run() -> int {
        let values = [|10, 20, 30, 40|]
        let selection = {{range}}
        let slice = values[selection]
        return slice.Length * 100 + slice[0] + slice[^1]
    }
}
""";
        Assert.Equal(expected, Run(source));
    }

    [Theory]
    [InlineData("..=^0", typeof(ArgumentOutOfRangeException))]
    [InlineData("..=-1", typeof(ArgumentOutOfRangeException))]
    [InlineData("..=2147483647", typeof(OverflowException))]
    public void InclusiveEnd_InvalidIndexCannotWrap(string range, Type exceptionType)
    {
        var source = $$"""
class Probe {
    public static func Run() -> int {
        let values = [|10, 20|]
        let slice = values[{{range}}]
        return slice.Length
    }
}
""";
        var error = Assert.Throws<TargetInvocationException>(() => Run(source));
        Assert.IsType(exceptionType, error.InnerException);
    }

    [Fact]
    public void LoopsCollectionsPatternsAndSlices_AgreeAtEnd()
    {
        var source = """
class Probe {
    public static func Run() -> int {
        var exclusiveCount = 0
        var inclusiveCount = 0
        for value in 1..3 { exclusiveCount += 1 }
        for value in 1..=3 { inclusiveCount += 1 }
        let exclusive: int[] = [1..3]
        let inclusive: int[] = [1..=3]
        let values = [|0, 1, 2, 3|]
        if exclusiveCount != 2 || inclusiveCount != 3 { return 1 }
        if exclusive.Length != 2 || inclusive.Length != 3 { return 2 }
        if values[1..3].Length != 2 || values[1..=3].Length != 3 { return 3 }
        if 3 is 1..3 { return 4 }
        if 3 is 1..=3 { return 42 }
        return 5
    }
}
""";
        Assert.Equal(42, Run(source));
    }

    private static int Run(string source)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("range-endpoints", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        return (int)loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null)!;
    }
}
