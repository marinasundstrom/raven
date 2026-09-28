using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ParameterPatternCodeGenTests
{
    [Theory]
    [InlineData("static func Sum((x, y): (int, int)) -> int { x + y }", "Sum((4, 6))")]
    [InlineData("static func Sum((x, y): (int, int)) -> int => x + y", "Sum((4, 6))")]
    [InlineData("static func Sum([x, y]: int[2]) -> int => x + y", "Sum([4, 6])")]
    [InlineData("static func Sum((x, y): (int, int)) -> int { let f = () => x + y; f() }", "Sum((4, 6))")]
    public void NamedPatternParameter_ExecutesWithOneMetadataParameter(string declaration, string call)
    {
        var source = $$"""
class C {
    {{declaration}}
    static func Run() -> int => {{call}}
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
        var type = loaded.Assembly.GetType("C")!;
        var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;
        Assert.Equal(10, type.GetMethod("Run", flags)!.Invoke(null, null));
        var parameter = Assert.Single(type.GetMethod("Sum", flags)!.GetParameters());
        Assert.Equal("<arg0>", parameter.Name);
        Assert.True(parameter.IsDefined(typeof(System.Runtime.CompilerServices.CompilerGeneratedAttribute)));
    }

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
