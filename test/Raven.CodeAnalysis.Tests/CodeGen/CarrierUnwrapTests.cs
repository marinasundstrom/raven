using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CarrierUnwrapTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ChainingExtractsPayloadOnceAndFailureDoesNotContinue(bool fail)
    {
        const string source = """
            import System.*
            class Harness {
                public static var Count: int = 0
                private static func Read(fail: bool) -> Result<string, string> {
                    Count += 1
                    if fail { return .Failure("bad") }
                    return .Ok("abc")
                }
                private static func Wrap(value: int) -> Optional<int> {
                    Count += 10
                    return .Some(value)
                }
                public static func Run(fail: bool) -> int => Wrap(Read(fail)!.Length)!
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("unwrap", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var unwraps = tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>().ToArray();
        Assert.Contains(unwraps, node => model.GetTypeInfo(node).Type?.SpecialType == SpecialType.System_String);
        Assert.Contains(unwraps, node => model.GetTypeInfo(node).Type?.SpecialType == SpecialType.System_Int32);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0403");
        using var pe = new MemoryStream();
        var emitted = compilation.Emit(pe);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        var type = loaded.Assembly.GetType("Harness")!;
        if (fail)
        {
            var exception = Assert.Throws<TargetInvocationException>(() => type.GetMethod("Run")!.Invoke(null, [true]));
            Assert.IsType<InvalidOperationException>(exception.InnerException);
        }
        else
        {
            Assert.Equal(3, type.GetMethod("Run")!.Invoke(null, [false]));
        }
        Assert.Equal(fail ? 1 : 11, type.GetProperty("Count")!.GetValue(null));
    }

    [Theory]
    [InlineData("Optional<int>", ".None")]
    [InlineData("Result<int, string>", ".Failure(\"bad\")")]
    public void EmptyCarrierFailsOutsideCarrierReturningFunction(string carrier, string value)
    {
        var source = $$"""
            import System.*
            class Harness {
                public static func Run() -> int {
                    let value: {{carrier}} = {{value}}
                    return value!
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("unwrap-failure", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var emitted = compilation.Emit(pe);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        var exception = Assert.Throws<TargetInvocationException>(() => loaded.Assembly.GetType("Harness")!.GetMethod("Run")!.Invoke(null, null));
        Assert.IsType<InvalidOperationException>(exception.InnerException);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void CustomCarrierAndConventionalUnionUnwrap(bool contract)
    {
        var declaration = contract ? """
            record struct Attempt(Value: int) : System.IPropagatable<Attempt, int, string> {
                func TryGetOutput(out output: int) -> bool { output = Value; return true }
                func TryGetResidual(out residual: string) -> bool { residual = "bad"; return false }
                static func FromResidual(residual: string) -> Attempt => Attempt(0)
            }
            """ : "union Result<T, E> { case Ok(Value: T); case Failure(Error: E) }";
        var expression = contract ? "Attempt(42)!" : "Get()!";
        var factory = contract ? "" : "private static func Get() -> Result<int, string> => .Ok(42)";
        var source = $$"""
            import System.*
            {{declaration}}
            class Harness {
                {{factory}}
                public static func Run() -> int => {{expression}}
            }
            """;
        var references = contract ? TestMetadataReferences.DefaultWithRavenCore : TestMetadataReferences.Default;
        var compilation = Compilation.Create("unwrap-custom", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var pe = new MemoryStream();
        var emitted = compilation.Emit(pe);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(pe, references);
        Assert.Equal(42, loaded.Assembly.GetType("Harness")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void CompletionAfterUnwrapUsesPayloadMembers()
    {
        const string source = """
            import System.*
            func Test(value: Result<string, int>) {
                value!.
            }
            """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("unwrap-completion", [tree], TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var position = source.IndexOf("value!.", StringComparison.Ordinal) + "value!.".Length;
        Assert.Contains(compilation.GetCompletions(tree, position), item => item.DisplayText == "Length");
    }
}
