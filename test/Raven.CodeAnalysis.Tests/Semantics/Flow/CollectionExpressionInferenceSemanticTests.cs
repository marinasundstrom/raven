using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class CollectionExpressionInferenceSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void SpreadInference_WithoutTargetType_UsesImmutableListDefault()
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
    public void SpreadInference_WithoutTargetType_MultipleSpreadsSameConcreteType_UsesImmutableListDefault()
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
    public void SpreadInference_WithoutTargetType_MultipleConcreteSpreadTypes_UsesImmutableListDefault()
    {
        const string source = """
import System.Collections.Immutable.*
import System.Collections.Generic.*

val a: ImmutableList<int> = [1, 2]
val b: List<int> = [3, 4]
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
    public void NoTargetType_NumericElements_InferMostPreciseCompatibleNumericList()
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
        var immutableListType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", immutableListType.MetadataName);
        Assert.Equal(SpecialType.System_Double, immutableListType.TypeArguments.Single().SpecialType);
    }

    [Fact]
    public void NoTargetType_PlainCollectionLiteral_InfersImmutableList()
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
        var immutableListType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", immutableListType.MetadataName);
        Assert.Equal("Int32", immutableListType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_DictionaryLiteral_InfersImmutableDictionary()
    {
        const string source = """
val inferred = ["a": 1, "b": 2]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundDictionaryExpression>(model.GetBoundNode(collection));
        var dictionaryType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableDictionary`2", dictionaryType.MetadataName);
        Assert.Equal(SpecialType.System_String, dictionaryType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryType.TypeArguments[1].SpecialType);
    }

    [Fact]
    public void NoTargetType_MutableDictionaryLiteral_InfersDictionary()
    {
        const string source = """
val inferred = !["a": 1, "b": 2]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundDictionaryExpression>(model.GetBoundNode(collection));
        var dictionaryType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("Dictionary`2", dictionaryType.MetadataName);
        Assert.Equal(SpecialType.System_String, dictionaryType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryType.TypeArguments[1].SpecialType);
    }

    [Fact]
    public void NoTargetType_DictionarySpreadLiteral_InfersImmutableDictionary()
    {
        const string source = """
import System.Collections.Generic.*

val other: Dictionary<string, int> = !["b": 2]
val inferred = [..."a": 1, ...other, "c": 3]
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
            .Single(static expression => expression.Elements.Any(static element => element is DictionarySpreadElementSyntax or SpreadElementSyntax));

        var bound = Assert.IsType<BoundDictionaryExpression>(model.GetBoundNode(collection));
        var dictionaryType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableDictionary`2", dictionaryType.MetadataName);
        Assert.Equal(SpecialType.System_String, dictionaryType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryType.TypeArguments[1].SpecialType);
    }

    [Fact]
    public void NoTargetType_DictionaryComprehension_InfersImmutableDictionary()
    {
        const string source = """
val inferred = [for key in [|"a", "bb"|] => key: key.Length]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundDictionaryExpression>(model.GetBoundNode(collection));
        var dictionaryType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableDictionary`2", dictionaryType.MetadataName);
        Assert.Equal(SpecialType.System_String, dictionaryType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryType.TypeArguments[1].SpecialType);
    }

    [Fact]
    public void NoTargetType_SemicolonSeparatedCollectionLiteral_InfersImmutableList()
    {
        const string source = """
val inferred = [1; 2; 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_MutableCollectionLiteral_InfersMutableList()
    {
        const string source = """
val inferred = ![1, 2, 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("List`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_MutableListCollectionLiteral_InfersMutableList()
    {
        const string source = """
val inferred = ![1; 2; 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("List`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_ExplicitArrayLiteral_InfersFixedArray()
    {
        const string source = """
val inferred = [|1, 2, 3|]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<ArrayExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(3, arrayType.FixedLength);
    }

    [Fact]
    public void NoTargetType_ExplicitArrayLiteralWithSpread_PreservesArrayInference()
    {
        const string source = """
val prefix = [|1, 2|]
val inferred = [|...prefix, 3|]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot()
            .DescendantNodes()
            .OfType<ArrayExpressionSyntax>()
            .Single(static expression => expression.Elements.Any(element => element is SpreadElementSyntax));

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var arrayType = Assert.IsAssignableFrom<IArrayTypeSymbol>(bound.Type);
        Assert.True(arrayType.IsFixedArray);
        Assert.Equal(3, arrayType.FixedLength);
    }

    [Fact]
    public void ImmutableArrayTarget_PlainCollectionExpression_BindsToImmutableArray()
    {
        const string source = """
import System.Collections.Immutable.*

val values: ImmutableArray<int> = [1, 2, 3]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableArray`1", targetType.MetadataName);
        Assert.Equal("Int32", targetType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void ImmutableArrayTarget_ExpressionBodiedMethodCollectionExpression_BindsToImmutableArray()
    {
        const string source = """
import System.Collections.Immutable.*

class Factory {
    static func Create() -> ImmutableArray<int> => [1, 2, 3]
}
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<CollectionExpressionSyntax>().Single();

        var bound = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var targetType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableArray`1", targetType.MetadataName);
        Assert.Equal("Int32", targetType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void ArrayTarget_CommaSeparatedCollectionExpression_Binds()
    {
        const string source = """
val values: int[] = [1, 2, 3]
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void ArrayTarget_SemicolonSeparatedCollectionExpression_Binds()
    {
        const string source = """
val values: int[] = [1; 2; 3]
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void ListTarget_CommaSeparatedCollectionExpression_Binds()
    {
        const string source = """
import System.Collections.Generic.*

val values: List<int> = [1, 2, 3]
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void ListTarget_SemicolonSeparatedCollectionExpression_Binds()
    {
        const string source = """
import System.Collections.Generic.*

val values: List<int> = [1; 2; 3]
""";

        CreateVerifier(source).Verify();
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithSpread_InfersImmutableList()
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
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithFixedSpread_InfersImmutableList()
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
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithConstantRangeElement_InfersImmutableList()
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
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithExclusiveConstantRangeElement_InfersImmutableList()
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
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithMixedConstantRangeElement_InfersImmutableList()
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
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_CollectionLiteralWithConstRangeEndpoint_InfersImmutableList()
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
        var listType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableList`1", listType.MetadataName);
        Assert.Equal("Int32", listType.TypeArguments.Single().MetadataName);
    }

    [Fact]
    public void NoTargetType_ExplicitArrayLiteralWithConstRangeEndpoint_InfersFixedArray()
    {
        const string source = """
const MAX_VALUE = 10
val inferred = [|3..MAX_VALUE|]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var collection = tree.GetRoot().DescendantNodes().OfType<ArrayExpressionSyntax>().Single();

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
