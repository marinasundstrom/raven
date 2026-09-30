using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public sealed class ArrayTypeProviderTests
{
    [Theory]
    [InlineData(1)]
    [InlineData(2)]
    public void NonPeProviderOwnsInterfacesAndMembersForEachArrayRank(int rank)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var element = compilation.GetSpecialType(SpecialType.System_Int32);
        var provider = new ProviderType(compilation);
        var contract = new SourceNamedTypeSymbol("ArrayView", null!, TypeKind.Interface, compilation.Assembly,
            null, compilation.SourceGlobalNamespace, [], [], addAsMember: false);
        provider.Contracts = [contract, contract];
        var member = new SourceMethodSymbol("Item", element, [], contract, contract,
            compilation.SourceGlobalNamespace, [], [], isStatic: false);
        provider.Members = [member];
        var array = new ArrayTypeSymbol(provider, element, compilation.Assembly, null,
            compilation.SourceGlobalNamespace, [], rank);

        Assert.Same(member, Assert.Single(array.GetMembers("Item")));
        Assert.Same(contract, member.ContainingType);
        Assert.Same(array, provider.RequestedArray);
        Assert.Equal(rank, provider.RequestedArray!.Rank);
        Assert.Same(element, provider.RequestedArray.ElementType);
        Assert.Same(contract, Assert.Single(array.Interfaces));
        Assert.Same(contract, Assert.Single(array.AllInterfaces));
        Assert.Equal(1, provider.InterfaceQueries);
        Assert.Null(array.GetDocumentationComment());
    }

    [Fact]
    public void MissingProviderDoesNotInventDotNetCollectionInterfaces()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var element = compilation.GetSpecialType(SpecialType.System_Int32);
        var baseType = new SourceNamedTypeSymbol("ArrayBase", null!, TypeKind.Class, compilation.Assembly,
            null, compilation.SourceGlobalNamespace, [], [], addAsMember: false);
        var member = new SourceMethodSymbol("Length", element, [], baseType, baseType,
            compilation.SourceGlobalNamespace, [], [], isStatic: false);
        var array = new ArrayTypeSymbol(baseType, element, compilation.Assembly, null,
            compilation.SourceGlobalNamespace, []);

        Assert.Empty(array.Interfaces);
        Assert.Empty(array.AllInterfaces);
        Assert.Same(member, Assert.Single(array.GetMembers("Length")));
        Assert.Same(compilation.SourceGlobalNamespace.ContainingAssembly, array.ContainingAssembly);
        Assert.Same(compilation.SourceGlobalNamespace.ContainingModule, array.ContainingModule);
    }

    private sealed class ProviderType(Compilation compilation)
        : SourceNamedTypeSymbol("ArrayBase", compilation.GetSpecialType(SpecialType.System_Object), TypeKind.Class,
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [], addAsMember: false), IArrayTypeProvider
    {
        internal ImmutableArray<INamedTypeSymbol> Contracts { get; set; } = [];
        internal ImmutableArray<ISymbol> Members { get; set; } = [];
        internal IArrayTypeSymbol? RequestedArray { get; private set; }
        internal int InterfaceQueries { get; private set; }
        public ImmutableArray<INamedTypeSymbol> GetAdditionalInterfaces(IArrayTypeSymbol array)
        {
            RequestedArray = array;
            InterfaceQueries++;
            return Contracts;
        }
        public ImmutableArray<ISymbol> GetMembers(IArrayTypeSymbol array)
        {
            RequestedArray = array;
            return Members;
        }
    }
}
