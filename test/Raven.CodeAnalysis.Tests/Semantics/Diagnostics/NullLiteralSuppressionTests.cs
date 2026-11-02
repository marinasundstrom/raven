using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NullLiteralSuppressionTests : CompilationTestBase
{
    [Theory]
    [InlineData("string", "null")]
    [InlineData("int", "null!")]
    [Trait("Target", "Shared")]
    public void NullSuppression_DoesNotRelaxOrdinaryNullConversions(string target, string expression)
    {
        var (compilation, _) = CreateCompilation($"class Probe {{ static func Make() -> {target} => {expression} }}");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error && d.Id == "RAV1503");
    }

    [Theory]
    [InlineData("static func Make() -> Node => null!")]
    [InlineData("static func Make() -> Node { return null! }")]
    [InlineData("static func Make() -> Node { let value: Node = null!; return value }")]
    [InlineData("static func Make() -> Node { var value = Node(); value = null!; return value }")]
    [InlineData("static func Use(value: Node) {}\nstatic func Make() { Use(null!) }")]
    [Trait("Target", "Shared")]
    public void SuppressedNullLiteral_UsesReferenceTarget(string members)
    {
        var (compilation, tree) = CreateCompilation($$"""
            class Node {}
            class Probe {
                {{members}}
            }
            """);
        var expression = Assert.Single(tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>());
        var model = compilation.GetSemanticModel(tree);
        var type = model.GetTypeInfo(expression).Type;

        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0403");
        Assert.NotNull(type);
        Assert.Equal("Node", type.Name);
        Assert.False(type.IsNullable);
        Assert.Equal("Node", model.GetOperation(expression)?.Type?.Name);
        Assert.Equal("Node", model.GetTypeInfo(expression).Type?.Name);
    }
}
