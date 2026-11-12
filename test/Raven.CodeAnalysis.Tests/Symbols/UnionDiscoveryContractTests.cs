using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class UnionDiscoveryContractTests
{
    [Fact]
    public void NullAndOrdinaryTypesAreNotUnionsOrCases()
    {
        Assert.Null(UnionSymbolExtensions.TryGetUnion(null));
        Assert.Null(UnionSymbolExtensions.TryGetUnionCase(null));
        var ordinary = Make<NamedTypeSymbol>(SymbolKind.Type, "Ordinary");
        Set(ordinary, "get_IsUnion", false);
        Set(ordinary, "get_IsUnionCase", false);
        Assert.Null(UnionSymbolExtensions.TryGetUnion(ordinary));
        Assert.Null(UnionSymbolExtensions.TryGetUnionCase(ordinary));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DirectContractsRequireTheirClassificationFlag(bool enabled)
    {
        var union = Union("Choice");
        var variant = Case(union);
        Set(union, "get_IsUnion", enabled);
        Set(variant, "get_IsUnionCase", enabled);
        Assert.Same(enabled ? union : null, UnionSymbolExtensions.TryGetUnion(union));
        Assert.Same(enabled ? variant : null, UnionSymbolExtensions.TryGetUnionCase(variant));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConstructedTypesResolveTheirDefinitionOnlyWhenClassified(bool enabled)
    {
        var union = Union("Choice");
        var variant = Case(union);
        var constructedUnion = Make<NamedTypeSymbol>(SymbolKind.Type, "Choice");
        var constructedCase = Make<NamedTypeSymbol>(SymbolKind.Type, "Some");
        Set(constructedUnion, "get_IsUnion", enabled);
        Set(constructedUnion, "get_ConstructedFrom", union);
        Set(constructedCase, "get_IsUnionCase", enabled);
        Set(constructedCase, "get_ConstructedFrom", variant);
        Assert.Same(enabled ? union : null, UnionSymbolExtensions.TryGetUnion(constructedUnion));
        Assert.Same(enabled ? variant : null, UnionSymbolExtensions.TryGetUnionCase(constructedCase));
    }

    [Fact]
    public void ClassifiedTypesWithNoMatchingDefinitionReturnNull()
    {
        var named = Make<NamedTypeSymbol>(SymbolKind.Type, "Other");
        Set(named, "get_IsUnion", true);
        Set(named, "get_IsUnionCase", true);
        Set(named, "get_ConstructedFrom", named);
        Assert.Null(UnionSymbolExtensions.TryGetUnion(named));
        Assert.Null(UnionSymbolExtensions.TryGetUnionCase(named));
    }

    [Fact]
    public void DiagnosticUsesUnionParameterNamesAndNestedNamespaces()
    {
        var union = Union("Choice", "Inner", "Outer");
        Set(union, "get_TypeParameters", ImmutableArray.Create(Parameter("T", 0), Parameter("E", 1)));
        var variant = Case(union);
        Assert.Equal("Choice<T, E>.Some", UnionSymbolExtensions.FormatUnionCaseForDiagnostic(variant));
        Assert.Equal("Outer.Inner.Choice<T, E>.Some", UnionSymbolExtensions.FormatUnionCaseForDiagnostic(variant, true));
    }

    [Fact]
    public void AmbiguousNamesIncludeNamespacesOnlyForMatchingCarrierNames()
    {
        var first = Case(Union("Choice", "First"));
        var second = Case(Union("Choice", "Second"));
        Assert.Equal(("First.Choice.Some", "Second.Choice.Some"), UnionSymbolExtensions.FormatAmbiguousCasePair(first, second));
        var different = Case(Union("Other", "Third"));
        Assert.Equal(("Choice.Some", "Other.Some"), UnionSymbolExtensions.FormatAmbiguousCasePair(first, different));
    }

    [Fact]
    public void ConcreteArgumentsReplaceParameterNamesInDiagnostics()
    {
        var union = Union("Choice");
        Set(union, "get_TypeParameters", ImmutableArray.Create(Parameter("T", 0)));
        var variant = Case(union);
        var argument = Make<TypeSymbol>(SymbolKind.Type, "Int32", TypeKind.Struct);
        Set(argument, "get_IsTupleType", false);
        Set(argument, "get_SpecialType", SpecialType.System_Int32);
        Set(argument, "ToFullyQualifiedMetadataName", "System.Int32");
        Set(variant, "get_TypeArguments", ImmutableArray.Create(argument));
        Assert.Equal("Choice<int>.Some", UnionSymbolExtensions.FormatUnionCaseForDiagnostic(variant));
        Set(variant, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(Parameter("Renamed", 0)));
        Assert.Equal("Choice<T>.Some", UnionSymbolExtensions.FormatUnionCaseForDiagnostic(variant));
    }

    private static UnionSymbol Union(string name, params string[] namespaceParts)
    {
        var union = Make<UnionSymbol>(SymbolKind.Type, name);
        Set(union, "get_IsUnion", true);
        Set(union, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        NamespaceSymbol? ns = null;
        foreach (var part in namespaceParts.Reverse())
        {
            var next = Make<NamespaceSymbol>(SymbolKind.Namespace, part);
            Set(next, "get_IsGlobalNamespace", false);
            Set(next, "get_ContainingNamespace", ns);
            ns = next;
        }
        Set(union, "get_ContainingNamespace", ns);
        return union;
    }

    private static UnionCaseTypeSymbol Case(UnionSymbol union)
    {
        var variant = Make<UnionCaseTypeSymbol>(SymbolKind.Type, "Some");
        Set(variant, "get_IsUnionCase", true);
        Set(variant, "get_Union", union);
        Set(variant, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        return variant;
    }
}
