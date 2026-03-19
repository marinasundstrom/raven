using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class RangeAndIndexCodeGenTests
{
    [Fact]
    public void ArraySlice_WithRange_EmitsAndRuns()
    {
        var code = """
class SliceHarness {
    public static func Run() -> int {
        val values = [|10, 20, 30, 40|]
        val slice = values[1..3]
        return slice.Length + slice[0] + slice[1]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("SliceHarness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(52, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void ArrayAccess_WithIndexValue_EmitsAndRuns()
    {
        var code = """
import System.*

class IndexHarness {
    public static func Run() -> int {
        val values = [|1, 2, 3, 4|]
        val i: Index = ^1
        return values[i]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("IndexHarness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(4, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void CustomIndexer_WithRangeAndIndex_EmitsAndRuns()
    {
        var code = """
import System.*

class LocalBuffer {
    var data: int = 0

    public func GetSeed() -> int {
        data
    }

    public var self[i: Index]: int {
        get => 1;
    }

    public var self[r: Range]: int[] {
        get => [2, 3];
    }
}

class StringHarness {
    public static func Run() -> int {
        val buffer = LocalBuffer()
        val tail = buffer[1..3]
        val i: Index = ^1
        return tail[0] + tail[1] + buffer[i]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("StringHarness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(6, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void JaggedArrayLiteral_EmitsAndRuns()
    {
        var code = """
class JaggedHarness {
    public static func Run() -> int {
        val values: int[][] = [[|1, 2|], [|3, 4|]]
        return values[1][0] + values[0].Length
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("JaggedHarness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(5, (int)method!.Invoke(null, null)!);
    }

    [Fact]
    public void MultidimensionalArrayAccessAndAssignment_EmitAndRun()
    {
        var code = """
import System.*

class MatrixHarness {
    public static func Run() -> int {
        val matrix = (Array.CreateInstance(typeof(int), 2, 2) as int[,])!
        matrix[0, 0] = 10
        matrix[0, 1] = 20
        matrix[1, 0] = 30
        matrix[1, 1] = 40
        return matrix[1, 0] + matrix[0, 1]
    }
}
""";

        var syntaxTree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;

        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, references);
        var assembly = loaded.Assembly;
        var type = assembly.GetType("MatrixHarness", true);
        var method = type!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static);

        Assert.Equal(50, (int)method!.Invoke(null, null)!);
    }
}
