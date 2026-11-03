using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SiblingLocalFunctionClosureTests
{
    [Fact]
    public void StaticLocalFunctionsCanCallStaticSiblingsInEitherDeclarationOrder()
    {
        var tree = SyntaxTree.ParseText("""
            public class Probe {
                static func Run() -> int {
                    return Middle()
                    static func First() -> int => 20
                    static func Middle() -> int => First() + Last()
                    static func Last() -> int => 22
                }
            }
            """);
        var compilation = Compilation.Create("StaticSiblingCalls", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("value")]
    [InlineData("Sibling()")]
    public void StaticLocalFunctionsStillRejectEnclosingState(string expression)
    {
        var tree = SyntaxTree.ParseText($$"""
            public class Probe {
                static func Run() -> int {
                    let value = 42
                    func Sibling() -> int => value
                    static func Read() -> int => {{expression}}
                    return Read()
                }
            }
            """);
        var compilation = Compilation.Create("StaticLocalCapture", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0103");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SameNamedFunctionsInDifferentMethodsRetainTheirOwnBodies(bool capture)
    {
        var firstBody = capture ? "var value = 10; func Read() -> int => value" : "func Read() -> int => 10";
        var secondBody = capture ? "var value = 20; func Read() -> int => value" : "func Read() -> int => 20";
        var tree = SyntaxTree.ParseText($$"""
            public class Probe {
                static func First() -> int { {{firstBody}}; return Read() }
                static func Second() -> int { {{secondBody}}; return Read() }
            }
            """);
        var compilation = Compilation.Create("DistinctLocalFunctions", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var functions = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>()
            .Select(node => model.GetDeclaredSymbol(node)).ToArray();
        Assert.Equal(2, functions.Length);
        Assert.False(SymbolEqualityComparer.Default.Equals(functions[0], functions[1]));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Probe")!;
        Assert.Equal(10, type.GetMethod("First")!.Invoke(null, null));
        Assert.Equal(20, type.GetMethod("Second")!.Invoke(null, null));
    }

    [Fact]
    public void SiblingsShareFrameWhenOneCapturesOnlySelf()
    {
        var tree = SyntaxTree.ParseText("""
            public class Probe {
                private val Value: int = 42
                private func Read() -> int => Value
                func Run() -> int {
                    var result = 0
                    func SetResult() { result = ReadValue() }
                    func ReadValue() -> int => self.Read()
                    SetResult()
                    return result
                }
            }
            """);
        var compilation = Compilation.Create("SiblingLocalClosure", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        var type = loaded.Assembly.GetType("Probe")!;
        Assert.Equal(42, type.GetMethod("Run")!.Invoke(Activator.CreateInstance(type), null));
    }
}
