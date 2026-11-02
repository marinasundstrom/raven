using System;
using System.IO;
using System.Linq;
using System.Reflection;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Operations;

namespace Raven.CodeAnalysis.Tests;

public class ConditionalAccessChainTests
{
    [Theory]
    [InlineData("value?.Trim().Length", "  abc  ", 3)]
    [InlineData("value?.ToCharArray()[1].ToString().Length", "abc", 1)]
    public void Chain_ShortCircuitsAndReturnsValue(string expression, string input, int expected)
    {
        var source = "class Probe { static func Run(value: string?) -> int => " + expression + " ?? -1 }";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ConditionalChain", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!;
        Assert.Equal(-1, method.Invoke(null, new object?[] { null }));
        Assert.Equal(expected, method.Invoke(null, new object?[] { input }));
    }

    [Fact]
    public void Chain_EvaluatesReceiverOnceAndSkipsArgumentsWhenNull()
    {
        const string source = """
            class Probe {
                static var Calls: int = 0
                static func Receiver(value: string?) -> string? { Calls += 1; return value }
                static func Index() -> int { Calls += 10; return 1 }
                static func Run(value: string?) -> int {
                    Calls = 0
                    let length = Receiver(value)?.Substring(Index()).Length ?? -1
                    return Calls * 100 + length
                }
                static func Discard(value: string?) -> int {
                    Calls = 0
                    Receiver(value)?.Substring(Index()).Trim()
                    return Calls
                }
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ConditionalChainEvaluation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!;
        Assert.Equal(99, method.Invoke(null, new object?[] { null }));
        Assert.Equal(1102, method.Invoke(null, new object?[] { "abc" }));
        var discard = loaded.Assembly.GetType("Probe", true)!.GetMethod("Discard")!;
        Assert.Equal(1, discard.Invoke(null, new object?[] { null }));
        Assert.Equal(11, discard.Invoke(null, new object?[] { "abc" }));
    }

    [Fact]
    public void Chain_NestedNullableAccessShortCircuitsEachReceiver()
    {
        const string source = """
            class Leaf { val Width: int => 4 }
            class Root { val Child: Leaf?; init(child: Leaf?) { Child = child } }
            class Probe {
                static func Read(root: Root?) -> int => root?.Child?.Width ?? -1
                static func Run() -> int => Read(null) + Read(Root(null)) + Read(Root(Leaf()))
            }
            """;
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("NestedConditionalChain", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(2, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void Chain_SyntaxOwnsMemberInvocationAndIndexTrailers()
    {
        const string source = "class Probe { static func Run(value: string?) => value?.ToCharArray()[0].ToString() }";
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        var conditional = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ConditionalAccessExpressionSyntax>());
        var invocation = Assert.IsType<InvocationExpressionSyntax>(conditional.WhenNotNull);
        var member = Assert.IsType<MemberAccessExpressionSyntax>(invocation.Expression);
        Assert.IsType<ElementAccessExpressionSyntax>(member.Expression);
        Assert.Equal(source, tree.GetRoot().ToFullString());
    }

    [Fact]
    public void Chain_ColdSemanticQueryReportsMemberTypeAndConditionalInstance()
    {
        var tree = SyntaxTree.ParseText("class Probe { static func Run(value: string?) => value?.Trim().Length }");
        var compilation = Compilation.Create("ConditionalChainSemantics", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(TestMetadataReferences.Default);
        var model = compilation.GetSemanticModel(tree);
        var member = tree.GetRoot().DescendantNodes().OfType<MemberAccessExpressionSyntax>().Single();
        Assert.Equal(SpecialType.System_Int32, model.GetTypeInfo(member).Type!.SpecialType);
        var conditional = tree.GetRoot().DescendantNodes().OfType<ConditionalAccessExpressionSyntax>().Single();
        var operation = Assert.IsAssignableFrom<IConditionalAccessOperation>(model.GetOperation(conditional));
        Assert.NotNull(operation.WhenNotNull);
        Assert.Contains(Descendants(operation), op => op is IConditionalAccessInstanceOperation && op.IsImplicit);
    }

    private static System.Collections.Generic.IEnumerable<IOperation> Descendants(IOperation operation)
    {
        foreach (var child in operation.ChildOperations)
        {
            yield return child;
            foreach (var descendant in Descendants(child))
                yield return descendant;
        }
    }

    [Theory]
    [InlineData("Optional<string>", ".Some(\" abc \")", "Optional<int>", ".None")]
    [InlineData("Result<string, string>", ".Ok(\" abc \")", "Result<int, string>", ".Failure(\"bad\")")]
    public void CarrierChain_MapsTheWholeBranch(string carrier, string value, string mapped, string empty)
    {
        var source = $$"""
            import System.*
            class Probe {
                static func Map(value: {{carrier}}) -> {{mapped}} => value?.Trim().Length
                static func Run(fail: bool) -> int {
                    let input: {{carrier}} = if fail { {{empty}} } else { {{value}} }
                    return Map(input)!
                }
            }
            """;
        var references = TestMetadataReferences.DefaultWithRavenCore;
        var compilation = Compilation.Create("CarrierChain", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var run = loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!;
        Assert.Equal(3, run.Invoke(null, new object[] { false }));
        var exception = Assert.Throws<TargetInvocationException>(() => run.Invoke(null, new object[] { true }));
        Assert.IsType<InvalidOperationException>(exception.InnerException);
    }

    [Fact]
    public void Chain_UnwrapsNullableValueReceiverOnce()
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("NullableValueChain", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("class Probe { static func Run(value: int?) -> int => value?.ToString().Length ?? -1 }"))
            .AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!;
        Assert.Equal(-1, method.Invoke(null, new object?[] { null }));
        Assert.Equal(2, method.Invoke(null, new object?[] { 12 }));
    }

    [Fact]
    public void Parentheses_EndConditionalChain()
    {
        var compilation = Compilation.Create("ConditionalBoundary", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText("class Probe { static func Run(value: string?) -> int => (value?.Trim()).Length }"))
            .AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0402");
    }
}
