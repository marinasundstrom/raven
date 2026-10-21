using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class UnionCaptureCodeGenTests
{
    [Theory]
    [InlineData("Some<object>(value)")]
    [InlineData("Some<object>(value + \"\")")]
    [InlineData("Some<object>(Wrap(value))")]
    public void UnionPayload_PreservesCapturedLocal_AndRuns(string expression)
    {
        var source = $$"""
import Option.*
union Option<T> {
    case Some(value: T)
    case None
}
class Harness {
    static func Wrap(value: string) -> string => value
    public static func Run() -> string {
        let value = "captured"
        let make: () -> Option<object> = () => {{expression}}
        return match make() {
            Some(let result) => (string)result
            None => "missing"
        }
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("UnionCapture", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(references);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes()
            .Single(node => node.Kind == SyntaxKind.ParenthesizedFunctionExpression);
        Assert.Contains(model.GetCapturedVariables(lambda), symbol => symbol.Name == "value");
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal("captured", loaded.Assembly.GetType("Harness")!.GetMethod("Run")!.Invoke(null, null));
    }
    [Theory]
    [InlineData("Box(value)")]
    [InlineData("Box() { Value = value }")]
    public void ObjectConstruction_PreservesCapturedLocal_AndRuns(string expression)
    {
        var source = $$"""
class Box {
    var Value: string
    init() { Value = "" }
    init(value: string) { Value = value }
}
class Harness {
    public static func Run() -> string {
        let value = "captured"
        let make: () -> Box = () => {{expression}}
        return make().Value
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ObjectCapture", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(references);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes()
            .Single(node => node.Kind == SyntaxKind.ParenthesizedFunctionExpression);
        Assert.Contains(model.GetCapturedVariables(lambda), symbol => symbol.Name == "value");
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal("captured", loaded.Assembly.GetType("Harness")!.GetMethod("Run")!.Invoke(null, null));
    }
}
