using System.IO;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class TuplePatternCodeGenTests
{
    [Fact]
    public void MatchExpression_WithTuplePattern_EmitsSuccessfully()
    {
        const string code = """
import System.*

func describe(value: object) -> string {
    match value {
        (first: int, second: int) => "${first + second}"
        _ => "no match"
    }
}

func main() {
    let value: object = (1, 2)
    Console.WriteLine(describe(value))
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_emit", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void LetTuplePatternAssignment_EmitsSuccessfully()
    {
        const string code = """
import System.*

func main() {
    let (first, second) = (1, 2)
    Console.WriteLine(first)
    Console.WriteLine(second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("let_tuple_pattern_emit", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void TuplePatternAssignment_WithExistingLocals_EmitsSuccessfully()
    {
        const string code = """
import System.*

func main() {
    var first = 0
    var second = 0
    (first, second, _) = (1, 2, 3)
    Console.WriteLine(first + second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_existing_locals", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void VarTuplePatternAssignment_EmitsSuccessfully()
    {
        const string code = """
import System.*

func main() {
    var (first, second, _) = (1, 2, 3)
    Console.WriteLine(first + second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_var_assignment", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }

    [Fact]
    public void MixedTuplePatternAssignment_EmitsSuccessfully()
    {
        const string code = """
import System.*

func main() {
    (let first, var second: double, _) = (1, 2, 3)
    Console.WriteLine(first)
    Console.WriteLine(second)
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("tuple_pattern_mixed_assignment", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);
    }
}
