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

    [Fact]
    public void CoverageTraversesSharedAndCyclicSealedBranchesOnce()
    {
        var root = Branch("Root", true, true);
        var middle = Branch("Middle", true, false);
        var open = Branch("Open", false, true);
        Set(root, "get_PermittedDirectSubtypes", ImmutableArray.Create(middle, open));
        Set(middle, "get_PermittedDirectSubtypes", ImmutableArray.Create(root, open));
        ((IdentityProxy)(object)root).ReadOnce.Add("get_PermittedDirectSubtypes");

        var coverage = TypeCoverageHelper.GetSealedHierarchyCoverageTypes(root);
        Assert.Equal(2, coverage.Length);
        Assert.Contains(middle, coverage);
        Assert.Contains(open, coverage);
    }

    [Fact]
    public void ConcreteGenericRootIsCoveredAsItsConstructedType()
    {
        var root = Branch("Root", true, false);
        Set(root, "get_IsGenericType", true);
        Set(root, "get_TypeParameters", ImmutableArray.Create(Parameter("T", 0)));
        Set(root, "get_PermittedDirectSubtypes", default(ImmutableArray<NamedTypeSymbol>));
        var projected = Branch("Root", true, false);
        Set(projected, "get_OriginalDefinition", root);
        Set(projected, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(Branch("Element", false, false)));

        Assert.Same(projected, Assert.Single(TypeCoverageHelper.GetSealedHierarchyCoverageTypes(root, projected)));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void IndirectProjectionRequiresConsistentRepeatedArguments(bool consistent)
    {
        var parameter = Parameter("T", 0);
        var leaf = Branch("Leaf", false, false);
        Set(leaf, "get_IsGenericType", true);
        Set(leaf, "get_TypeParameters", ImmutableArray.Create(parameter));
        var root = Branch("Root", true, true);
        Set(root, "get_TypeParameters", ImmutableArray.Create(Parameter("A", 0), Parameter("B", 1)));
        var template = Branch("Root", true, true);
        Set(template, "get_OriginalDefinition", root);
        Set(template, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(parameter, parameter));
        var middle = Branch("Middle", true, true);
        Set(middle, "get_BaseType", template);
        Set(leaf, "get_BaseType", middle);

        var element = Branch("Element", false, false);
        var hierarchy = Branch("Root", true, true);
        Set(hierarchy, "get_OriginalDefinition", root);
        Set(hierarchy, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(element, consistent ? element : middle));
        var constructed = Branch("Leaf", false, false);
        Set(leaf, "Construct", (object?[]? arguments) =>
        {
            Assert.Same(element, Assert.Single(Assert.IsType<TypeSymbol[]>(Assert.Single(arguments!))));
            return (object?)constructed;
        });

        Assert.Same(consistent ? constructed : leaf, SealedHierarchyFacts.ProjectCaseTypeToHierarchyArguments(leaf, hierarchy));
    }

    private static NamedTypeSymbol Branch(string name, bool closed, bool abstractType)
    {
        var type = Named("A", $"Example.{name}");
        Set(type, "get_IsSealedHierarchy", closed);
        Set(type, "get_IsAbstract", abstractType);
        Set(type, "get_Arity", 0);
        Set(type, "get_Interfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        return type;
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
