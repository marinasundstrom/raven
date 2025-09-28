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
}
