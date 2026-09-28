using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ParameterPatternCodeGenTests
{
    [Theory]
    [InlineData("record class", "{x: let horizontal, y: let vertical}", "horizontal + vertical")]
    [InlineData("record struct", "{x: let horizontal, y: let vertical}", "horizontal + vertical")]
    [InlineData("record class", "{x: let horizontal}", "horizontal + 6")]
    public void PropertyParameter_ExtractsMembers(string kind, string pattern, string body)
    {
        var source = $$"""
{{kind}} Point(x: int, y: int)
class C {
    static func Sum({{pattern}}: Point) -> int => {{body}}
    static func Run() -> int => Sum(Point(4, 6))
}
""";
        Assert.Equal(10, CompileAndRunParameterSample(source));
    }

    [Fact]
    public void PropertyParameter_ExtractsStructFields()
    {
        const string source = """
struct Point { public field x: int; public field y: int }
class C {
    static func Sum({x: let x, y: let y}: Point) -> int => x + y
    static func Run() -> int {
        var point = Point()
        point.x = 4
        point.y = 6
        Sum(point)
    }
}
""";
        Assert.Equal(10, CompileAndRunParameterSample(source));
    }

    [Fact]
    public void PropertyParameter_EvaluatesEachGetterOnceIncludingDiscard()
    {
        const string source = """
class Point {
    public field Reads: int
    public val x: int { get { self.Reads += 1; return 4 } }
    public val y: int { get { self.Reads += 1; return 6 } }
}
class C {
    static func Read({x: let x, y: _}: Point) -> int => x
    static func Run() -> int {
        let point = Point()
        let value = Read(point)
        value * 10 + point.Reads
    }
}
""";
        Assert.Equal(42, CompileAndRunParameterSample(source));
    }

    [Fact]
    public void PropertyLambda_ExtractsNestedSequence()
    {
        const string source = """
record class Row(Items: int[])
class C {
    static func Run() -> int {
        let read: (Row) -> int = ({Items: [..items]}) => items.Length
        read(Row([1, 2, 3]))
    }
}
""";
        Assert.Equal(3, CompileAndRunParameterSample(source));
    }

    private static object? CompileAndRunParameterSample(string source)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("test", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(System.Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        return loaded.Assembly.GetType("C")!.GetMethod("Run", BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static)!.Invoke(null, null);
    }

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
