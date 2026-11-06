using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class GenericBoundsContractTests
{
    [Theory, Trait("Target", "Shared")]
    [InlineData(true)]
    [InlineData(false)]
    public void EveryUnionAlternativeMustSatisfyAtLeastOneBound(bool covered)
    {
        var a = Type("A");
        var b = Type("B");
        var first = Type("First", a);
        var second = Type("Second", covered ? b : null);
        Assert.Equal(covered, SemanticFacts.SatisfiesTypeConstraint(Union(first, second), Union(a, b)));
    }

    [Theory, Trait("Target", "NeoCLR")]
    [InlineData(true)]
    [InlineData(false)]
    public void IntersectionBoundsRequireEveryConstituent(bool implementsBoth)
    {
        var a = Type("A");
        var b = Type("B");
        Set(a, "get_TypeKind", TypeKind.Interface);
        Set(b, "get_TypeKind", TypeKind.Interface);
        var source = Type("Source");
        Set(source, "get_AllInterfaces", implementsBoth ? ImmutableArray.Create(a, b) : ImmutableArray.Create(a));
        var target = Make<IntersectionTypeSymbol>(SymbolKind.Type, "intersection", TypeKind.Intersection);
        Set(target, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(a, b));
        Assert.Equal(implementsBoth, SemanticFacts.SatisfiesTypeConstraint(source, target));
        Assert.True(SemanticFacts.SatisfiesTypeConstraint(target, a));
    }

    [Theory, Trait("Target", "Shared")]
    [InlineData(false)]
    [InlineData(true)]
    public void NamedBoundsFollowTransitiveParameterConstraints(bool isInterface)
    {
        var bound = Type("Bound");
        if (isInterface)
            Set(bound, "get_TypeKind", TypeKind.Interface);
        var outer = Parameter("T", 0);
        var inner = Parameter("U", 1);
        Set(outer, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(outer, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(inner));
        Set(inner, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(bound));
        Assert.True(SemanticFacts.SatisfiesNamedTypeConstraint(outer, bound));
    }

    [Theory, Trait("Target", "Shared")]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void CombinedConstraintsCheckEachRequirement(bool allowedRefLike, bool hasBound)
    {
        var bound = Type("Bound");
        Set(bound, "get_TypeKind", TypeKind.Interface);
        var source = Type("Value");
        Set(source, "get_TypeKind", TypeKind.Struct);
        Set(source, "get_AllInterfaces", hasBound ? ImmutableArray.Create(bound) : ImmutableArray<NamedTypeSymbol>.Empty);
        Set(source, "get_IsRefLikeType", true);
        var parameter = Parameter("T", 0);
        var flags = TypeParameterConstraintKind.TypeConstraint;
        if (allowedRefLike)
            flags |= TypeParameterConstraintKind.AllowByRefLike;
        Set(parameter, "get_ConstraintKind", flags);
        Set(parameter, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(bound));
        Assert.Equal(allowedRefLike && hasBound, source.SatisfiesConstraints(parameter));
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void FunctionConstraintRecognizesStructuralSignaturesAndForwardedBounds()
    {
        var function = Make<IFunctionType>(SymbolKind.Type, "function");
        var parameter = Parameter("T", 0);
        Set(parameter, "get_ConstraintKind", TypeParameterConstraintKind.FunctionType);
        Assert.True(SemanticFacts.SatisfiesFunctionTypeConstraint(function));
        Assert.True(SemanticFacts.SatisfiesFunctionTypeConstraint(parameter));
        Assert.False(SemanticFacts.SatisfiesFunctionTypeConstraint(Type("Delegate")));
    }

    [Fact, Trait("Target", "Shared")]
    public void ForwardedUnionRequiresCoverageOfAllDeclaredAlternatives()
    {
        var a = Type("A");
        var b = Type("B");
        var source = Parameter("T", 0);
        Set(source, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(Union(a, Type("Unrelated"))));
        Assert.False(SemanticFacts.SatisfiesTypeConstraint(source, Union(a, b)));
    }

    [Fact, Trait("Target", "Shared")]
    public void RecursiveStructuralProofStopsAtTheDepthLimit()
    {
        var cycle = Union();
        Set(cycle, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(cycle));
        Assert.False(SemanticFacts.SatisfiesTypeConstraint(cycle, Type("Unrelated")));
    }

    [Fact, Trait("Target", "Shared")]
    public void ErrorTypesAvoidCascadesAndAbsentArgumentsCannotSatisfyBounds()
    {
        var error = Make<ErrorTypeSymbol>(SymbolKind.Type, "?", TypeKind.Error);
        var parameter = Parameter("T", 0);
        Assert.True(error.SatisfiesConstraints(parameter));
        Assert.True(SemanticFacts.SatisfiesTypeConstraint(Type("Known"), error));
        Assert.False(SemanticFacts.SatisfiesConstraints(null, parameter));
        Assert.False(error.SatisfiesConstraints(null));
    }

    private static StructuralUnionTypeSymbol Union(params TypeSymbol[] members)
    {
        var union = Make<StructuralUnionTypeSymbol>(SymbolKind.Type, "union", TypeKind.StructuralUnion);
        Set(union, "get_ConstituentTypes", members.ToImmutableArray());
        return union;
    }

    private static NamedTypeSymbol Type(string name, NamedTypeSymbol? parent = null)
    {
        var type = Named("Library", "Example." + name);
        Set(type, "get_BaseType", parent);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }

    internal interface IFunctionType : TypeSymbol, IStructuralFunctionIdentity { }
}
