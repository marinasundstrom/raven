using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class BacktickIdentifierCodeGenTests
{
    [Fact]
    public void BacktickMethod_EmitsExactMetadataNameAndRuns()
    {
        var tree = SyntaxTree.ParseText("""
            class Calculator {
                public static func `Calculate total price`(price: int, quantity: int) -> int {
                    return price * quantity
                }
                public static func Run() -> int => Calculator.`Calculate total price`(10, 5)
            }
            """);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("BacktickNames", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Calculator", true)!;
        Assert.NotNull(type.GetMethod("Calculate total price"));
        Assert.Equal(50, type.GetMethod("Run")!.Invoke(null, null));
    }
}
