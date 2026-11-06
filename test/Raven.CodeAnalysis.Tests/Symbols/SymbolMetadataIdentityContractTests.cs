using System.Collections.Immutable;

using Raven.CodeAnalysis;

using Xunit;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "DotNet")]
public sealed class SymbolMetadataIdentityContractTests
{
    [Fact]
    public void NullAndReferenceIdentityFollowTheOriginalContract()
    {
        var type = Named("A", "Example.Token");
        Assert.True(SymbolExtensions.MetadataIdentityEquals(null, null));
        Assert.True(SymbolExtensions.MetadataIdentityEquals(type, type));
        Assert.False(SymbolExtensions.MetadataIdentityEquals(type, null));
    }

    [Fact]
    public void NominalMetadataIdentityCanAgreeAcrossAssemblyOwners()
    {
        var left = Named("A", "Example.Token");
        var right = Named("B", "Example.Token");
        Assert.False(SymbolEqualityComparer.Default.Equals(left, right));
        Assert.True(SymbolExtensions.MetadataIdentityEquals(left, right));
        Assert.False(SymbolExtensions.MetadataIdentityEquals(left, Named("B", "Example.Other")));
    }

    [Theory]
    [InlineData(null, null, true)]
    [InlineData(3, 3, true)]
    [InlineData(3, 4, false)]
    [InlineData(null, 3, false)]
    public void ArrayMetadataIdentityPreservesFixedLengths(int? leftLength, int? rightLength, bool expected)
    {
        var left = Array(Named("A", "Example.Token"), leftLength);
        var right = Array(Named("B", "Example.Token"), rightLength);
        Assert.Equal(expected, SymbolExtensions.MetadataIdentityEquals(left, right));
    }

    [Fact]
    public void StructuralArgumentsDoNotFallBackToNominalMetadataNames()
    {
        var leftUnion = Make<StructuralUnionTypeSymbol>(SymbolKind.Type, "union", TypeKind.StructuralUnion);
        var rightUnion = Make<StructuralUnionTypeSymbol>(SymbolKind.Type, "union", TypeKind.StructuralUnion);
        Set(leftUnion, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(Named("A", "Example.Left"), Named("A", "Example.Shared")));
        Set(rightUnion, "get_ConstituentTypes", ImmutableArray.Create<TypeSymbol>(Named("A", "Example.Right"), Named("A", "Example.Shared")));
        var left = Constructed("A", leftUnion);
        var right = Constructed("B", rightUnion);
        Assert.False(SymbolExtensions.MetadataIdentityEquals(left, right));
    }

    private static NamedTypeSymbol Constructed(string assemblyName, TypeSymbol argument)
    {
        var definition = Named(assemblyName, "Example.Box`1");
        var type = Named(assemblyName, "Example.Box`1");
        Set(type, "get_IsGenericType", true);
        Set(type, "get_ConstructedFrom", definition);
        Set(type, "get_OriginalDefinition", definition);
        Set(type, "get_TypeArguments", ImmutableArray.Create(argument));
        return type;
    }

    private static ArrayTypeSymbol Array(TypeSymbol element, int? length)
    {
        var type = Make<ArrayTypeSymbol>(SymbolKind.Type, "array", TypeKind.Array);
        Set(type, "get_ElementType", element);
        Set(type, "get_Rank", 1);
        Set(type, "get_FixedLength", length);
        Set(type, "get_IsFixedArray", length.HasValue);
        return type;
    }

}
