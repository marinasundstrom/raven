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
        const string source = "val value = ^2";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var indexSyntax = tree.GetRoot().DescendantNodes().OfType<IndexExpressionSyntax>().Single();
        var boundIndex = model.GetBoundNode(indexSyntax).ShouldBeOfType<BoundIndexExpression>();

        boundIndex.IsFromEnd.ShouldBeTrue();
        boundIndex.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("Index");
        boundIndex.Value.ShouldBeOfType<BoundLiteralExpression>().Value.ShouldBe(2);
    }

    [Fact]
    public void RangeExpression_TracksEndpoints()
    {
        const string source = "val range = ^2..^0";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var rangeSyntax = tree.GetRoot().DescendantNodes().OfType<RangeExpressionSyntax>().Single();
        var boundRange = model.GetBoundNode(rangeSyntax).ShouldBeOfType<BoundRangeExpression>();

        boundRange.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("Range");

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
val values = [1, 2, 3]
val last = values[^1]
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

    [Fact]
    public void ArrayAccess_WithRange_BindsToSubArrayInvocation()
    {
        const string source = """
val values = [1, 2, 3, 4]
val middle = values[1..3]
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var elementAccess = tree.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().Single();
        var boundAccess = model.GetBoundNode(elementAccess).ShouldBeOfType<BoundInvocationExpression>();

        boundAccess.Method.Name.ShouldBe("GetSubArray");
        boundAccess.Method.ContainingType.ToFullyQualifiedMetadataName().ShouldBe("System.Runtime.CompilerServices.RuntimeHelpers");
        boundAccess.Type.ShouldNotBeNull();
        var arrayType = boundAccess.Type.ShouldBeAssignableTo<IArrayTypeSymbol>();
        arrayType.ElementType.SpecialType.ShouldBe(SpecialType.System_Int32);
    }

    [Fact]
    public void IndexerResolution_UsesSystemIndexAndSystemRangeArgumentTypes()
    {
        const string source = """
import System.*

val buffer = Buffer()
val tail = buffer[1..]
val i = ^1
val last = buffer[i]

class Buffer {
    public self[i: Index]: int {
        get => 1;
    }

    public self[r: Range]: int[] {
        get => [2, 3];
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        diagnostics.ShouldBeEmpty();

        var model = compilation.GetSemanticModel(tree);
        var accesses = tree.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().ToArray();
        accesses.Length.ShouldBe(2);

        var rangeAccess = model.GetBoundNode(accesses[0]).ShouldBeOfType<BoundIndexerAccessExpression>();
        rangeAccess.Indexer.GetMethod!.Parameters.Length.ShouldBe(1);
        rangeAccess.Indexer.GetMethod.Parameters[0].Type.ToFullyQualifiedMetadataName().ShouldBe("System.Range");

        var indexAccess = model.GetBoundNode(accesses[1]).ShouldBeOfType<BoundIndexerAccessExpression>();
        indexAccess.Indexer.GetMethod!.Parameters.Length.ShouldBe(1);
        indexAccess.Indexer.GetMethod.Parameters[0].Type.ToFullyQualifiedMetadataName().ShouldBe("System.Index");
    }
}
