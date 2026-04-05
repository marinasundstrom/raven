using System.IO;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class PatternVariableTests
{
    [Fact]
    public void PatternVariableInIfCondition_EmitsSuccessfully()
    {
        var code = """
import System.*

class Program {
    static func Main() -> unit {
        val v = if true { 1; } else { 2; }
        if v is int a {
            Console.WriteLine(a)
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree)
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
    }
}
