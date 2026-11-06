using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SealedHierarchyLookupTests
{
    [Fact]
    public void CyclicBoundLeavesOtherBoundsAvailableAndPreservesConstructedType()
    {
        var root = Named("A", "Example.Root");
        Set(root, "get_IsSealedHierarchy", true);
        var constructed = Named("A", "Example.Root");
        Set(constructed, "get_OriginalDefinition", root);
        var parameter = Parameter("T", 0);
        Set(parameter, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(parameter, constructed));

        Assert.True(TypeCoverageHelper.TryGetSealedHierarchy(parameter, out var definition, out var projected));
        Assert.Same(root, definition);
        Assert.Same(constructed, projected);
    }

    [Theory]
    [InlineData("Leaf", true)]
    [InlineData("leaf", false)]
    public void DirectCaseLookupUsesOrdinalNames(string name, bool matches)
    {
        var root = Named("A", "Example.Root");
        var leaf = Named("A", "Example.Leaf");
        Set(root, "get_IsSealedHierarchy", false);
        Set(root, "get_PermittedDirectSubtypes", ImmutableArray.Create(leaf));

        Assert.True(SealedHierarchyFacts.TryGetCaseDefinition(root, name, out var definition, out var result));
        Assert.Same(root, definition);
        Assert.Same(matches ? leaf : null, result);
    }

    [Fact]
    public void EmptySealedHierarchyAndMissingHierarchyHaveDistinctResults()
    {
        var root = Named("A", "Example.Root");
        Set(root, "get_IsSealedHierarchy", true);
        Set(root, "get_PermittedDirectSubtypes", default(ImmutableArray<NamedTypeSymbol>));
        Assert.True(SealedHierarchyFacts.TryGetCaseDefinition(root, "Leaf", out var definition, out var result));
        Assert.Same(root, definition);
        Assert.Null(result);

        Set(root, "get_IsSealedHierarchy", false);
        Assert.False(SealedHierarchyFacts.TryGetCaseDefinition(root, "Leaf", out definition, out result));
        Assert.Null(definition);
        Assert.Null(result);
    }
}
