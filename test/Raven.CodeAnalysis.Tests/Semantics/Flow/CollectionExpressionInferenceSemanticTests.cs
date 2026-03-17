using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class CollectionExpressionInferenceSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void SpreadInference_WithoutTargetType_PreservesSingleConcreteSpreadType()
    {
        const string source = """
import System.Collections.Immutable.*

val list: ImmutableList<int> = [2, 3, 4]
val newList = [7, ...list, 5]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);

        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(static expression => expression.Elements.Any(element => element is SpreadElementSyntax) && expression.Elements.Count > 1);

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        Assert.Equal("ImmutableList<int>", bound.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void SpreadInference_WithoutTargetType_MultipleSpreadsSameConcreteType_PreservesType()
    {
        const string source = """
import System.Collections.Immutable.*

val a: ImmutableList<int> = [1, 2]
val b: ImmutableList<int> = [3, 4]
val merged = [...a, ...b]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);

        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(static expression => expression.Elements.Count == 2 && expression.Elements.All(element => element is SpreadElementSyntax));

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        Assert.Equal("ImmutableList<int>", bound.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void SpreadInference_WithoutTargetType_MultipleConcreteSpreadTypes_ReportsDiagnostic()
    {
        const string source = """
import System.Collections.Immutable.*
import System.Collections.Generic.*

val a: ImmutableList<int> = [1, 2]
val b: List<int> = [3, 4]
val merged = [...a, ...b]
""";

        var verifier = CreateVerifier(source, [
            new DiagnosticResult("RAV2027").WithAnySpan().WithArguments("ImmutableList<int>", "List<int>")
        ]);

        verifier.Verify();
    }

    [Fact]
    public void SpreadInference_ExplicitTargetType_WinsOverConflictingSpreadTypes()
    {
        const string source = """
import System.Collections.Immutable.*
import System.Collections.Generic.*

val a: ImmutableList<int> = [1, 2]
val b: List<int> = [3, 4]
val merged: List<int> = [...a, ...b]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);

        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(static expression => expression.Elements.Count == 2 && expression.Elements.All(element => element is SpreadElementSyntax));

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        Assert.Equal("List<int>", bound.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void NoTargetType_NumericElements_InferMostPreciseCompatibleNumericArray()
    {
        const string source = """
val inferred = [1, 2.0]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);

        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        Assert.Equal("double[2]", bound.Type.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat));
    }

    [Fact]
    public void NoTargetType_PlainCollectionLiteral_InfersFixedSizeArray()
    {
        const string source = """
val inferred = [1, 2, 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(3, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithSpread_InfersOpenArray()
    {
        const string source = """
val values: int[] = [1, 2]
val inferred = [...values, 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(static expression => expression.Elements.Any(element => element is SpreadElementSyntax));

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.False(arrayType.IsFixedArray);
        Assert.Null(arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithFixedSpread_InfersFixedArray()
    {
        const string source = """
val values: int[2] = [1, 2]
val inferred = [...values, 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<CollectionExpressionSyntax>()
            .Single(static expression => expression.Elements.Any(element => element is SpreadElementSyntax));

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(3, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithConstantRangeElement_InfersFixedArray()
    {
        const string source = """
val inferred = [1..3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(3, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithExclusiveConstantRangeElement_InfersFixedArray()
    {
        const string source = """
val inferred = [1..<4]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(3, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithMixedConstantRangeElement_InfersFixedArray()
    {
        const string source = """
val inferred = [1, 3..4, 9]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(4, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithConstRangeEndpoint_InfersFixedArray()
    {
        const string source = """
const MAX_VALUE = 10
val inferred = [3..MAX_VALUE]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(8, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_IncompatibleElements_DoNotInferObjectArray()
    {
        const string source = """
val inferred = [1, true]
""";

        var verifier = CreateVerifier(source, [
            new DiagnosticResult("RAV1503").WithAnySpan().WithArguments("int", "bool")
        ]);

        verifier.Verify();
    }
}
