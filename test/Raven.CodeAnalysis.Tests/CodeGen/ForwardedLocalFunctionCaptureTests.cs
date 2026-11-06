using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ForwardedLocalFunctionCaptureTests
{
    [Theory]
    [InlineData("func Leaf() -> int => value; func Middle() -> int => Leaf(); return Middle()")]
    [InlineData("func Middle() -> int => Leaf(); func Leaf() -> int => value; return Middle()")]
    [InlineData("func Leaf() -> int => value; func Middle() -> int => Leaf(); func Top() -> int => Middle(); return Top()")]
    [InlineData("func A(n: int) -> int => if n == 0 { value } else { B(n - 1) }; func B(n: int) -> int => A(n); return B(2)")]
    [InlineData("var output = 0; func Leaf() { output = 42 }; func Middle() { Leaf() }; Middle(); return output")]
    [InlineData("func Leaf() -> string => value.ToString(); func Middle() -> int { let read: () -> object = Leaf; return if read() is string text && text == \"42\" { 42 } else { 0 } }; return Middle()")]
    [InlineData("func Leaf<T>(item: T) -> int => value; func Middle() -> int => Leaf<int>(0); return Middle()")]
    [InlineData("func Leaf() -> int => value; func Middle() -> int { let read: () -> int = Leaf; return read() }; return Middle()")]
    public void DotNet_SiblingCallsForwardEnclosingParameter(string body)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ static func Run(value: int) -> int {{ {body} }} }}");
        var compilation = Compilation.Create("ForwardCaptures", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [42]));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_SiblingCapturesAreAvailableBeforeAndAfterDiagnostics(bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Run(value: int) -> int {
                    func Middle() -> int => Leaf()
                    func Leaf() -> int => value
                    return Middle()
                }
            }
            """);
        var compilation = Compilation.Create("CaptureQuery", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var middle = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().First();
        if (diagnosticsFirst)
            Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var declared = Assert.IsAssignableFrom<MethodSymbol>(model.GetDeclaredSymbol(middle));
        Assert.Equal("value", Assert.Single(model.GetCapturedVariables(declared)).Name);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Equal("value", Assert.Single(model.GetCapturedVariables(middle)).Name);
    }
    [Fact, Trait("Target", "DotNet")]
    public void NestedCalleeLocalsAreKeptInTheirOwnInvocation()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Run(seed: int) -> int {
                    func Read() -> int => Compute()
                    func Compute() -> int {
                        var local = seed
                        func Nested() -> int => local
                        return Nested()
                    }
                    return Read()
                }
            }
            """);
        var compilation = Compilation.Create("NestedCalls", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var read = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().First();
        var symbols = model.GetCapturedVariables(read);
        Assert.Equal(new[] { "seed" }, symbols.Select(s => s.Name));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [42]));
    }

    [Theory]
    [InlineData("local", 45)]
    [InlineData("local + seed", 78)]
    [InlineData("seed + local", 78)]
    public void DotNet_EscapingLocalFunctionsRetainPerCallStorage(string read, int expected)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static func Run(seed: int) -> int {
                    func Make(offset: int) -> () -> int {
                        var local = seed + offset
                        func Read() -> int => {{read}}
                        return Read
                    }
                    let first = Make(1)
                    let second = Make(2)
                    return first() * 10 + second()
                }
            }
            """);
        var compilation = Compilation.Create("EscapingLocal", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [3]));
    }

    [Theory]
    [InlineData("shared += 1")]
    [InlineData("Increment()")]
    public void DotNet_EscapedFunctionsShareOuterWritesAndIsolateInnerWrites(string increment)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static func Run(seed: int) -> int {
                    var shared = seed
                    func Increment() { shared += 1 }
                    func Make(offset: int) -> () -> int {
                        var local = offset
                        func Next() -> int {
                            local += 1
                            {{increment}}
                            return local * 10 + shared
                        }
                        return Next
                    }
                    let first = Make(1)
                    let second = Make(4)
                    shared = 10
                    return first() * 10000 + second() * 100 + first()
                }
            }
            """);
        var compilation = Compilation.Create("EscapedWrites", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(316243, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [3]));
    }

    [Theory]
    [InlineData("Read", "func Read() -> int => outer + middle + inner")]
    [InlineData("() => outer + middle + inner", "")]
    public void DotNet_EscapingCallbackReadsThreeLexicalScopes(string callback, string nestedDeclaration)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Probe {
                static func Run(outer: int) -> int {
                    func First() -> () -> int {
                        var middle = 2
                        func Second() -> () -> int {
                            var inner = 3
                            {{nestedDeclaration}}
                            let callback: () -> int = {{callback}}
                            return callback
                        }
                        let saved = Second()
                        middle = 9
                        return saved
                    }
                    return First()()
                }
            }
            """);
        var compilation = Compilation.Create("ThreeScopes", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [30]));
    }

    [Fact]
    public void DotNet_ReentryKeepsEachLocalFunctionsOwnStorage()
    {
        var tree = SyntaxTree.ParseText("""
            class Probe {
                static func Run(seed: int) -> int {
                    func Sum(depth: int) -> int {
                        var local = seed + depth
                        func Read() -> int => local
                        if depth == 0 { return Read() }
                        let prior = Sum(depth - 1)
                        return prior + Read()
                    }
                    return Sum(2)
                }
            }
            """);
        var compilation = Compilation.Create("Reentry", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, [13]));
    }
}
