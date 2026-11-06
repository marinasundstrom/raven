using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class InterfaceConstraintContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void CyclicBoundsStillCheckIndependentInterfaceConstraints(bool hasInterface)
    {
        var target = Interface("IMarker");
        var outer = Parameter("T", 0);
        var nested = Parameter("U", 1);
        Set(outer, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(nested));
        Set(nested, "get_ConstraintTypes", hasInterface
            ? ImmutableArray.Create<TypeSymbol>(outer, target)
            : ImmutableArray.Create<TypeSymbol>(outer));
        Assert.Equal(hasInterface, outer.ImplementsInterface(target));
    }

    [Fact]
    public void NominalQueriesRespectTheInterfaceClosureAndTargetKind()
    {
        var target = Interface("IMarker");
        var type = Named("A", "Example.Concrete");
        Set(type, "get_AllInterfaces", ImmutableArray.Create(target));
        Assert.True(type.ImplementsInterface(target));
        Assert.True(target.ImplementsInterface(target));
        Assert.False(type.ImplementsInterface(Interface("IOther")));
        Assert.False(SemanticFacts.ImplementsInterface(null, target));
        Assert.False(type.ImplementsInterface(null));
        Assert.Equal("interfaceType", Assert.Throws<ArgumentException>(() => type.ImplementsInterface(type)).ParamName);
    }

    [Theory]
    [InlineData(VarianceKind.Out, false, true)]
    [InlineData(VarianceKind.Out, true, false)]
    [InlineData(VarianceKind.In, false, false)]
    [InlineData(VarianceKind.In, true, true)]
    [InlineData(VarianceKind.None, false, false)]
    public void VarianceFollowsTheReferenceConversionDirection(VarianceKind variance, bool reverse, bool expected)
    {
        var root = Named("A", "Example.Root");
        var derived = Named("A", "Example.Derived");
        Set(root, "get_IsReferenceType", true);
        Set(derived, "get_IsReferenceType", true);
        Set(root, "get_BaseType", null);
        Set(derived, "get_BaseType", root);
        var parameter = Parameter("T", 0);
        Set(parameter, "get_Variance", variance);
        var definition = Interface("IContract`1");
        Set(definition, "get_TypeParameters", ImmutableArray.Create(parameter));
        var source = Construct(definition, reverse ? root : derived);
        var target = Construct(definition, reverse ? derived : root);
        Assert.Equal(expected, source.ImplementsInterface(target));
    }

    [Theory]
    [InlineData(false, true, true)]
    [InlineData(true, false, false)]
    [InlineData(true, true, true)]
    public void CovariancePreservesReferenceAnnotations(bool nullableSource, bool nullableTarget, bool expected)
    {
        var root = Named("A", "Example.Root");
        var derived = Named("A", "Example.Derived");
        Set(root, "get_IsReferenceType", true);
        Set(derived, "get_IsReferenceType", true);
        Set(derived, "get_BaseType", root);
        var definition = CovariantDefinition();
        var source = Construct(definition, nullableSource ? Annotated(derived) : derived);
        var target = Construct(definition, nullableTarget ? Annotated(root) : root);
        Assert.Equal(expected, source.ImplementsInterface(target));
    }

    [Theory]
    [InlineData(true, true)]
    [InlineData(false, false)]
    public void CovarianceRequiresAReferenceConversionToAnImplementedInterface(bool referenceType, bool expected)
    {
        var marker = Interface("IMarker");
        Set(marker, "get_IsReferenceType", true);
        var element = Named("A", "Example.Element");
        Set(element, "get_IsReferenceType", referenceType);
        Set(element, "get_TypeKind", referenceType ? TypeKind.Class : TypeKind.Struct);
        Set(element, "get_BaseType", null);
        Set(element, "get_AllInterfaces", ImmutableArray.Create(marker));
        var definition = CovariantDefinition();
        Assert.Equal(expected, Construct(definition, element).ImplementsInterface(Construct(definition, marker)));
    }

    private static NamedTypeSymbol CovariantDefinition()
    {
        var parameter = Parameter("T", 0);
        Set(parameter, "get_Variance", VarianceKind.Out);
        var definition = Interface("IProducer`1");
        Set(definition, "get_TypeParameters", ImmutableArray.Create(parameter));
        return definition;
    }

    private static NamedTypeSymbol Construct(NamedTypeSymbol definition, TypeSymbol argument)
    {
        var constructed = Interface(definition.Name);
        Set(constructed, "get_IsGenericType", true);
        Set(constructed, "get_OriginalDefinition", definition);
        Set(constructed, "get_ConstructedFrom", definition);
        Set(constructed, "get_TypeArguments", ImmutableArray.Create(argument));
        return constructed;
    }

    private static NamedTypeSymbol Interface(string name)
    {
        var type = Named("A", "Example." + name);
        Set(type, "get_TypeKind", TypeKind.Interface);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }
}
