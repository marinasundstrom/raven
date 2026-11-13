using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SubstitutedTypeParameterContractTests
{
    [Fact]
    public void SubstitutedInterfacesSupportArbitraryCountsAndDeduplicateInheritedInterfaces()
    {
        var parent = Named("Parent", TypeKind.Interface);
        var child = Named("Child", TypeKind.Interface);
        Set(child, "get_AllInterfaces", ImmutableArray.Create(parent));
        var parameter = new SubstitutedNamedTypeParameterSymbol(Original(), [child, child, parent]);
        Assert.Equal(new[] { child, parent }, parameter.Interfaces);
        Assert.Equal(new[] { child, parent }, parameter.AllInterfaces);
        Assert.Equal(parameter.Interfaces, parameter.Interfaces);
        Assert.Equal(parameter.AllInterfaces, parameter.AllInterfaces);
        var empty = new SubstitutedNamedTypeParameterSymbol(Original(), []);
        Assert.Empty(empty.Interfaces);
        Assert.Empty(empty.AllInterfaces);
    }

    [Fact]
    public void ClassSubstitutionDeterminesBaseTypeAndKnownReferenceType()
    {
        var original = Original();
        var node = Named("Node", TypeKind.Class);
        var substituted = new SubstitutedNamedTypeParameterSymbol(original, [node]);
        Assert.Same(node, substituted.BaseType);
        Assert.True(substituted.IsReferenceType);
        Assert.Same(original, substituted.OriginalDefinition);
        Assert.Equal(original.Ordinal, substituted.Ordinal);
        Assert.Equal(original.Variance, substituted.Variance);
        Assert.Equal(original.OwnerKind, substituted.OwnerKind);
    }

    private static SourceTypeParameterSymbol Original()
        => new("T", Named("Owner", TypeKind.Class), null, null, [], [], 0, 0, [], VarianceKind.Out);

    private static NamedTypeSymbol Named(string name, TypeKind kind)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, kind);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        Set(type, "get_IsNullable", false);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }
}
