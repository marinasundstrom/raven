using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ParameterPatternCodeGenTests
{
    [Theory]
    [InlineData("class", 10)]
    [InlineData("struct", 9)]
    public void NominalParameter_CallsDeconstructOnceAndPreservesValueCopy(string kind, int expected)
    {
        var source = $$"""
{{kind}} Counter {
    public field Calls: int
    public func Deconstruct(out value: int) -> unit {
        self.Calls += 1
        value = 9
    }
}
class C {
    static func Read(Counter(let value): Counter) -> int => value
    static func Run() -> int {
        var counter = Counter()
        let result = Read(counter)
        result + counter.Calls
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("C")!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!;
        Assert.Equal(expected, method.Invoke(null, null));
    }

    [Theory]
    [InlineData("static func Sum(Row(let x, let y): Row) -> int => x + y", "Sum(Row(4, 6))")]
    [InlineData("static func Sum(Row(x, y): Row) -> int { x + y }", "Sum(Row(4, 6))")]
    [InlineData("static func Sum((Row(x, y), _): (Row, int)) -> int => x + y", "Sum((Row(4, 6), 0))")]
    [InlineData("static func Sum(row: Row) -> int { let f: (Row) -> int = Row(let x, let y) => x + y; f(row) }", "Sum(Row(4, 6))")]
    public void NominalParameter_ExecutesDeconstruction(string declaration, string call)
    {
        var source = $$"""
record class Row(X: int, Y: int)
class C {
    {{declaration}}
    static func Run() -> int => {{call}}
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("C")!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!;
        Assert.Equal(10, method.Invoke(null, null));
    }

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
