using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Shouldly;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class CollectionComprehensionSemanticTests : DiagnosticTestBase
{
    [Fact]
    public void CollectionComprehension_WithFilter_BindsAsSpreadOfWhereSelect()
    {
        const string source = """
import System.*
import System.Linq.*

val numbers = [1, 2, 3, 4]
val result = [for n in numbers if n % 2 == 0 => n * n]
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
            .Single(c => c.Elements.Any(e => e is CollectionComprehensionElementSyntax));

        var boundCollection = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var spread = Assert.IsType<BoundSpreadElement>(boundCollection.Elements.Single());
        var comprehension = Assert.IsType<BoundCollectionComprehensionExpression>(spread.Expression);
        Assert.NotNull(comprehension.Condition);
    }

    [Fact]
    public void CollectionComprehension_WithoutFilter_BindsAsSpreadOfSelect()
    {
        const string source = """
import System.*
import System.Linq.*

val numbers = [1, 2, 3, 4]
val result = [for n in numbers => n * n]
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
            .Single(c => c.Elements.Any(e => e is CollectionComprehensionElementSyntax));

        var boundCollection = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var spread = Assert.IsType<BoundSpreadElement>(boundCollection.Elements.Single());
        var comprehension = Assert.IsType<BoundCollectionComprehensionExpression>(spread.Expression);
        Assert.Null(comprehension.Condition);
    }

    [Fact]
    public void CollectionComprehension_WithRangeSource_BindsAsRangeSource()
    {
        const string source = """
import System.*
import System.Linq.*

val result = [for n in 4..250 if n % 2 == 0 => n * n]
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
            .Single(c => c.Elements.Any(e => e is CollectionComprehensionElementSyntax));

        var boundCollection = Assert.IsType<BoundCollectionExpression>(model.GetBoundNode(collection));
        var spread = Assert.IsType<BoundSpreadElement>(boundCollection.Elements.Single());
        var comprehension = Assert.IsType<BoundCollectionComprehensionExpression>(spread.Expression);
        var rangeSource = Assert.IsType<BoundRangeExpression>(comprehension.Source);
        Assert.NotNull(rangeSource.Left);
        Assert.NotNull(rangeSource.Right);
    }

    [Fact]
    public void CollectionComprehension_WithDeconstructionTarget_BindsPatternLocals()
    {
        const string source = """
val people = [(1, "Ada"), (2, "Bob")]
val names = [for val (id, name) in people if id > 1 => name]
""";

        var verifier = CreateVerifier(source);
        var run = verifier.GetResult();

        Assert.Empty(run.UnexpectedDiagnostics);
        Assert.Empty(run.MissingDiagnostics);

        var tree = run.Compilation.SyntaxTrees.Single();
        var model = run.Compilation.GetSemanticModel(tree);
        var designation = tree.GetRoot()
            .DescendantNodes()
            .OfType<SingleVariableDesignationSyntax>()
            .Single(d => d.Identifier.ValueText == "name");

        var symbol = model.GetDeclaredSymbol(designation).ShouldBeOfType<SourceLocalSymbol>();
        symbol.Type.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).ShouldBe("string");
        symbol.IsMutable.ShouldBeFalse();
    }

    [Fact]
    public void DictionaryComprehension_WithDeconstructionTarget_InfersDictionaryShape()
    {
        const string source = """
val pairs = [("a", 1), ("bb", 2)]
val lengths = [for val (key, value) in pairs => key: value]
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
            .Single(c => c.Elements.Any(e => e is DictionaryComprehensionElementSyntax));

        var bound = Assert.IsType<BoundDictionaryExpression>(model.GetBoundNode(collection));
        var dictionaryType = Assert.IsAssignableFrom<INamedTypeSymbol>(bound.Type);
        Assert.Equal("ImmutableDictionary`2", dictionaryType.MetadataName);
        Assert.Equal(SpecialType.System_String, dictionaryType.TypeArguments[0].SpecialType);
        Assert.Equal(SpecialType.System_Int32, dictionaryType.TypeArguments[1].SpecialType);
    }
}
