using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class GenericFrameTests
{
    [Theory]
    [InlineData("static func Create<T>(seed: T) -> () -> T { func Make(arg: T) -> () -> T { var value = arg; func Read() -> T => value; return Read }; return Make(seed) }", "Create<int>(42)()")]
    [InlineData("static func Create<T>(seed: T) -> () -> T { func Make() -> () -> T { var value = seed; func Read() -> T => value; return Read }; return Make() }", "Create<int>(42)()")]
    [InlineData("static func Create(seed: int) -> () -> int { func Make<T>(arg: T) -> () -> int { var value = arg; func Read() -> int => if value is int v { v + seed } else { seed }; return Read }; return Make<int>(40) }", "Create(2)()")]
    [InlineData("static func Create<T>(outer: T) -> () -> int { func Make<U>(inner: U) -> () -> int { var value = inner; func Read() -> int => if value is int n && outer is string text { n + text.Length } else { 0 }; return Read }; return Make<int>(40) }", "Create<string>(\"ok\")()")]
    [InlineData("static func Create<T>(outer: T) -> () -> int { func Make<U>(first: T, second: U) -> () -> int { func Read() -> int => if first is string text && second is int n { n + text.Length } else { 0 }; return Read }; return Make<int>(outer, 40) }", "Create<string>(\"ok\")()")]
    public void DotNet_NestedGenericFramesPreserveRuntimeType(string method, string call)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ {method}; static func Run() -> int => {call} }}");
        var compilation = Compilation.Create("GenericFrames", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, null));
    }
}
