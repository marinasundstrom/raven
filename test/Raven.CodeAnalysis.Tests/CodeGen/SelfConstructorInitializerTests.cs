using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class SelfConstructorInitializerTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void QualifiedConstructorInDelegationResolvesAcrossSyntaxTrees(bool reverse)
    {
        var sources = new[] {
            """
            namespace Public {
                class Value {
                    val Number: int
                    init(number: int) : self(Internal.Value(number)) { }
                    init(value: Internal.Value) { Number = value.Number }
                }
            }
            class Probe { static func Run() -> int => Public.Value(42).Number }
            """,
            """
            namespace Public.Internal {
                class Value {
                    val Number: int
                    init(number: int) { Number = number }
                }
            }
            """
        };
        if (reverse) Array.Reverse(sources);
        var compilation = Compilation.Create("QualifiedDelegation" + Guid.NewGuid().ToString("N"),
            sources.Select(source => SyntaxTree.ParseText(source)).ToArray(), TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Equal(42, Execute(compilation));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MissingQualifiedConstructorInDelegationReportsDiagnostic(bool documentDiagnostics)
    {
        var tree = SyntaxTree.ParseText("namespace Public { class Value { init() : self(Internal.Missing()) {}; init(value: int) {} } }");
        var compilation = Compilation.Create("MissingDelegation", [tree,
            SyntaxTree.ParseText("namespace Public.Internal { class Present {} }; class Missing {}")],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = documentDiagnostics ? compilation.GetSemanticModel(tree).GetDiagnostics() : compilation.GetDiagnostics();
        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == "RAV0234");
        using var image = new MemoryStream();
        Assert.False(compilation.Emit(image).Success);
    }

    [Theory]
    [InlineData("class")]
    [InlineData("struct")]
    public void DelegationRunsFieldInitializerOnceAndBodiesInOrder(string kind)
    {
        var source = $$"""
            public static class Counter {
                static field Count: int
                static func Next() -> int { Count += 1; return Count }
            }
            public {{kind}} Value {
                public field Number: int = Counter.Next()
                init() : self(40) { Number += 2 }
                init(number: int) { Number += number }
            }
            public static class Probe {
                static func Run() -> int {
                    let value = Value()
                    if Counter.Count != 1 { return -1 }
                    return value.Number
                }
            }
            """;
        Assert.Equal(43, Execute(source));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ForwardAndBackwardOverloadsDelegateThroughBase(bool reverse)
    {
        var constructors = new[] {
            "init() : self(number: 40) { Number += 2 }",
            "init(number: int, unused: bool = false) : base(number) {}"
        };
        if (reverse) Array.Reverse(constructors);
        var source = """
            public open class Parent {
                public field Number: int
                init(number: int) { Number = number }
            }
            public class Child : Parent {
            """ + string.Join("\n", constructors) + """
            }
            public static class Probe {
                static func Run() -> int => Child().Number
            }
            """;
        Assert.Equal(42, Execute(source));
    }

    [Theory]
    [InlineData("init() : self() {}")]
    [InlineData("init() : self(1) {}; init(value: int) : self() {}")]
    public void RecursiveDelegationPreventsEmission(string members)
    {
        var compilation = Create("class Value { " + members + " }");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0376");
        using var image = new MemoryStream();
        Assert.False(compilation.Emit(image).Success);
        Assert.Equal(0, image.Length);
    }

    [Fact]
    public void DelegationCycleAcrossPartialDeclarationsIsDiagnosed()
    {
        var compilation = Compilation.Create("PartialCycle", [
            SyntaxTree.ParseText("partial class Value { init() : self(1) {} }"),
            SyntaxTree.ParseText("partial class Value { init(value: int) : self() {} }")
        ], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0376");
    }

    [Fact]
    public void GenericDelegationPreservesConstructedOwner()
    {
        Assert.Equal(42, Execute("""
            public class Box<T> {
                val Value: T
                init(value: T) : self(value, true) {}
                init(value: T, marker: bool) { Value = value }
            }
            public static class Probe {
                static func Run() -> int => Box<int>(42).Value
            }
            """));
    }

    [Theory]
    [InlineData("self.Number")]
    [InlineData("Number")]
    [InlineData("Read()")]
    public void DelegationArgumentsCannotReadTheUninitializedInstance(string expression)
    {
        var compilation = Create("class Value { field Number: int; func Read() -> int => Number; init() : self(" + expression + ") {}; init(number: int) {} }");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0377");
    }

    [Fact]
    public void StaticConstructorCannotDelegate()
    {
        var compilation = Create("class Value { static init() : self(1) {}; init(value: int) {} }");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0312");
    }

    [Fact]
    public void MissingOverloadProducesDiagnostic()
    {
        var compilation = Create("class Value { init() : self(1) {} }");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1501");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SemanticQueriesResolveDelegationWithoutBodyBinding(bool diagnosticsFirst)
    {
        var compilation = Create("class Value { init() : self(1) {}; init(value: int) {} }");
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        var initializer = tree.GetRoot().DescendantNodes().OfType<SelfConstructorInitializerSyntax>().Single();
        if (diagnosticsFirst) Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var selected = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(initializer).Symbol);
        Assert.Equal(MethodKind.Constructor, selected.MethodKind);
        Assert.Single(selected.Parameters);
        Assert.Same(selected, model.GetSymbolInfo(initializer).Symbol);
    }

    [Fact]
    public void SyntaxPreservesInitializerIdentityAndRoundTrips()
    {
        const string source = "class Value { init() : self(1) {}; init(value: int) {} }";
        var tree = SyntaxTree.ParseText(source);
        var initializer = Assert.Single(tree.GetRoot().DescendantNodes().OfType<SelfConstructorInitializerSyntax>());
        Assert.Equal(SyntaxKind.SelfKeyword, initializer.Keyword.Kind);
        Assert.Single(initializer.ArgumentList.Arguments);
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Theory]
    [InlineData("base")]
    [InlineData("self")]
    public void InitializerCollectionArgumentsUseConstructorParameterType(string initializer)
    {
        var declaration = initializer == "base"
            ? "class Value : Storage { init(left: Left, right: Right?) : base([left, right]) {} }"
            : "class Value : Storage { init(items: Node?[]) : base(items) {}; init(left: Left, right: Right?) : self([left, right]) {} }";
        var source = $$"""
            open class Node { }
            class Left : Node { }
            class Right : Node { }
            open class Storage {
                val Items: Node?[]
                init(items: Node?[]) { Items = items }
            }
            {{declaration}}
            static class Probe {
                static func Run() -> int {
                    let left = Left()
                    let value = Value(left, null)
                    if !System.Object.ReferenceEquals(value.Items[0], left) || value.Items[1] is not null { return -1 }
                    return value.Items.Length
                }
            }
            """;
        Assert.Equal(2, Execute(source));
    }

    private static Compilation Create(string source) => Compilation.Create(
        "SelfConstructor" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)],
        TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    private static int Execute(string source)
    {
        return Execute(Create(source));
    }

    private static int Execute(Compilation compilation)
    {
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        return (int)loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null)!;
    }
}
