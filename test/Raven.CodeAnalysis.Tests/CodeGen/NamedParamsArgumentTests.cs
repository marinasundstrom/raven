using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class NamedParamsArgumentTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("SourceCount(values: 1)", 1)]
    [InlineData("SourceCount(values: [1, 2])", 2)]
    [InlineData("SourceCount()", 0)]
    [InlineData("NamedParamsFixture.Count(values: 1)", 1)]
    [InlineData("NamedParamsFixture.Count(values: [1, 2])", 2)]
    [InlineData("NamedParamsFixture.Count(values: null)", -1)]
    [InlineData("NamedParamsFixture.Count()", 0)]
    [InlineData("NamedParamsFixture.Generic(values: 1)", 1)]
    [InlineData("NamedParamsFixture.Generic(1)", 1)]
    [InlineData("NamedParamsFixture.Generic(values: [1, 2])", 2)]
    [InlineData("NamedParamsFixture.Generic(values: NamedParamsFixture.Array())", 2)]
    [InlineData("NamedParamsFixture.Nullable(values: 1)", 1)]
    [InlineData("NamedParamsFixture.Nullable<int>(values: null)", -1)]
    [InlineData("NamedParamsFixture.Generic(values: List<int>())", 1)]
    [InlineData("SourceGeneric(values: 1)", 1)]
    [InlineData("NamedParamsFixture.Select(values: 1)", 1)]
    [InlineData("NamedParamsFixture.Select(values: \"text\")", 2)]
    public void DotNet_NamedArgumentChoosesNormalOrExpandedForm(string call, int expected)
    {
        var compilation = Create(call, TargetPlatform.DotNet);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNet_ExpandedNamedArgumentMatchesCSharp()
    {
        Assert.Equal(1, NamedParamsFixture.Count(values: 1));
        Assert.Equal(1, NamedParamsFixture.Generic(values: 1));
    }

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData("SourceCount(values: 1)")]
    [InlineData("SourceCount(values: [1, 2])")]
    [InlineData("SourceGeneric(values: 1)")]
    public void NeoCLR_NamedParamsBindingUsesSharedRules(string call)
    {
        var compilation = Create(call, TargetPlatform.NeoCLR);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var bound = Assert.IsType<BoundInvocationExpression>(model.GetBoundNode(invocation));
        Assert.Single(bound.Arguments);
        Assert.Contains(bound.Method.Name, new[] { "SourceCount", "SourceGeneric" });
    }

    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("SourceCount(values: 1, values: 2)")]
    [InlineData("SourceCount(1, values: 2)")]
    [InlineData("SourceCount(values: ...[1], values: ...[2])")]
    [InlineData("SourceCount(values: \"bad\")")]
    public void DotNet_DuplicateAndIncompatibleArgumentsRemainErrors(string call)
    {
        var compilation = Create(call, TargetPlatform.DotNet);
        Assert.DoesNotContain(compilation.SyntaxTrees.Single().GetDiagnostics(), diagnostic => diagnostic.Id != "RAV1008");
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static Compilation Create(string call, TargetPlatform target)
    {
        var source = $$"""
            import Raven.CodeAnalysis.Tests.*
            import System.Collections.Generic.*
            public class Probe {
                static func SourceCount(params values: int[]) -> int => values.Length
                static func SourceGeneric<T>(params values: T[]) -> int => values.Length
                static func Run() -> int => {{call}}
            }
            """;
        return Compilation.Create("NamedParamsProbe", [SyntaxTree.ParseText(source)],
            TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(NamedParamsFixture).Assembly.Location)).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
    }
}

public static class NamedParamsFixture
{
    public static int Select(params int[] values) => 1;
    public static int Select(params object[] values) => 2;
    public static int[] Array() => [1, 2];
    public static int Nullable<T>(params T[]? values) => values?.Length ?? -1;
    public static int Count(params int[]? values) => values?.Length ?? -1;
    public static int Generic<T>(params T[] values) => values.Length;
}
