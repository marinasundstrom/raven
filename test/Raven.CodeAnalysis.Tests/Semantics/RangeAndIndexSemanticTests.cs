using System.Linq;

using Microsoft.CodeAnalysis;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class RangeAndIndexSemanticTests : CompilationTestBase
{
    [Fact]
    public void IndexExpression_HasIndexTypeAndFromEndFlag()
    {
        const string source = "let value = ^2";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var indexSyntax = tree.GetRoot().DescendantNodes().OfType<IndexExpressionSyntax>().Single();
        var boundIndex = model.GetBoundNode(indexSyntax).ShouldBeOfType<BoundIndexExpression>();

        boundIndex.IsFromEnd.ShouldBeTrue();
        boundIndex.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("System.Index");
        boundIndex.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(2);
    }

    [Fact]
    public void RangeExpression_TracksEndpoints()
    {
        const string source = "let range = ^2..^0";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var rangeSyntax = tree.GetRoot().DescendantNodes().OfType<RangeExpressionSyntax>().Single();
        var boundRange = model.GetBoundNode(rangeSyntax).ShouldBeOfType<BoundRangeExpression>();

        boundRange.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("System.Range");

        boundRange.Left.ShouldNotBeNull();
        boundRange.Left!.IsFromEnd.ShouldBeTrue();
        boundRange.Left.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(2);

        boundRange.Right.ShouldNotBeNull();
        boundRange.Right!.IsFromEnd.ShouldBeTrue();
        boundRange.Right.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(0);
    }

    [Fact]
    public void ArrayAccess_UsesIndexExpression()
    {
        const string source = """
let values = [1, 2, 3]
let last = values[^1]
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var elementAccess = tree.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().Single();
        var boundElementAccess = model.GetBoundNode(elementAccess).ShouldBeOfType<BoundArrayAccessExpression>();

        var index = boundElementAccess.Indices.Single().ShouldBeOfType<BoundIndexExpression>();
        index.IsFromEnd.ShouldBeTrue();
        index.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(1);
    }
}
