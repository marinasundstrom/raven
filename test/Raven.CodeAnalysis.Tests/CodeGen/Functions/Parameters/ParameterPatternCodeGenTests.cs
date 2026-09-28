using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ParameterPatternCodeGenTests
{
    [Theory]
    [InlineData("int[2]", "[x, y]", "x + y", "[4, 6]", 10)]
    [InlineData("int[3]", "[head, ..tail]", "head + tail.Length", "[4, 6, 8]", 6)]
    [InlineData("int[]", "[..items]", "items.Length", "[]", 0)]
    [InlineData("int[]", "[..items]", "items.Length", "[4, 6, 8]", 3)]
    public void IrrefutableParameterPattern_Executes(
        string inputType, string pattern, string body, string argument, int expected)
    {
        var source = $$"""
class C {
    static func Run() -> int {
        let f: ({{inputType}}) -> int = ({{pattern}}) => {{body}}
        return f({{argument}})
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source))
            .AddReferences(references);

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("C")!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!;
        Assert.Equal(expected, method.Invoke(null, null));
    }
}
