using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class InheritanceConstraintContractTests
{
    [Fact]
    public void NominalDerivationStartsAtTheBaseAndUsesMetadataIdentity()
    {
        var root = Class("Root");
        var middle = Class("Middle", root);
        var leaf = Class("Leaf", middle);
        Assert.True(leaf.IsDerivedFrom(middle));
        Assert.True(leaf.IsDerivedFrom(Named("OtherAssembly", "Example.Root")));
        Assert.False(leaf.IsDerivedFrom(leaf));
        Assert.False(root.IsDerivedFrom(leaf));
        Assert.False(SemanticFacts.IsDerivedFrom(null, root));
        Assert.False(leaf.IsDerivedFrom(null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConstraintSearchVisitsEachParameterOnceAndChecksLaterBounds(bool hasBase)
    {
        var root = Class("Root");
        var outer = Parameter("T", 0);
        var nested = Parameter("U", 1);
        Set(outer, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(nested));
        Set(nested, "get_ConstraintTypes", hasBase
            ? ImmutableArray.Create<TypeSymbol>(outer, Class("Derived", root))
            : ImmutableArray.Create<TypeSymbol>(outer));
        Assert.Equal(hasBase, outer.IsDerivedFrom(root));
    }

    [Fact]
    public void RepeatedBaseDoesNotHideAnUnrelatedMissingType()
    {
        var first = Class("First");
        var second = Class("Second", first);
        Set(first, "get_BaseType", second);
        ((IdentityProxy)(object)first).ReadOnce.Add("get_BaseType");
        Assert.False(Class("Child", first).IsDerivedFrom(Class("Unrelated")));
    }

    [Fact]
    public void ReferenceAnnotationsOnTheConstraintAndTargetPreserveTheBaseRelation()
    {
        var root = Class("Root");
        var derived = Class("Derived", root);
        var parameter = Parameter("T", 0);
        Set(parameter, "get_ConstraintTypes", ImmutableArray.Create<TypeSymbol>(Annotated(derived)));
        Assert.True(parameter.IsDerivedFrom(Annotated(root)));
    }

    private static NamedTypeSymbol Class(string name, NamedTypeSymbol? parent = null)
    {
        var type = Named("A", "Example." + name);
        Set(type, "get_BaseType", parent);
        return type;
    }

}
