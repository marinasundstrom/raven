using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ArrayTypeContractTests
{
    [Theory]
    [InlineData(1, null, "[]")]
    [InlineData(1, 10, "[10]")]
    [InlineData(2, 10, "[,]")]
    public void RankAndFixedLengthPreserveShape(int rank, int? length, string suffix)
    {
        var element = Named("A", "Example.Value");
        var array = new ArrayTypeSymbolImplementation(null, element, null, null, null, [], rank, length);
        Assert.Same(element, array.ElementType);
        Assert.Equal(rank, array.Rank);
        Assert.Equal(rank == 1 ? length : null, array.FixedLength);
        Assert.Equal(rank == 1 && length.HasValue, array.IsFixedArray);
        Assert.Equal($"{element}{suffix}", array.Name);
        Assert.Equal(TypeKind.Array, array.TypeKind);
        Assert.Equal(SpecialType.System_Array, array.SpecialType);
        Assert.Null(array.ContainingAssembly);
        Assert.Null(array.ContainingModule);
        Assert.Empty(array.GetMembers());
        Assert.Empty(array.Interfaces);
        Assert.Empty(array.AllInterfaces);
    }

    [Fact]
    public void ProviderOwnsMembersAndCompleteInterfaceCache()
    {
        var element = Named("A", "Example.Value");
        var provider = CreateProvider();
        var contract = Named("A", "Example.Contract");
        Set(contract, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        var member = Make<FieldSymbol>(SymbolKind.Field, "Length");
        Set(provider, "GetMembers", ImmutableArray.Create<Symbol>(member));
        Set(provider, "GetAdditionalInterfaces", ImmutableArray.Create(contract, contract));
        var array = new ArrayTypeSymbolImplementation(provider, element, null, null, null, []);
        Assert.Same(member, Assert.Single(array.GetMembers("Length")));
        Assert.Same(contract, Assert.Single(array.Interfaces));
        Assert.Same(contract, Assert.Single(array.AllInterfaces));
        Set(provider, "GetAdditionalInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Assert.Same(contract, Assert.Single(array.Interfaces));
        Assert.Same(contract, Assert.Single(array.AllInterfaces));
    }

    [Fact]
    public void ProvisionalProviderInterfacesAreNotCached()
    {
        var element = Named("A", "Example.Value");
        var provider = CreateProvider();
        Set(provider, "get_AreInterfacesComplete", false);
        var array = new ArrayTypeSymbolImplementation(provider, element, null, null, null, []);
        Assert.Empty(array.Interfaces);
        Assert.Empty(array.AllInterfaces);
        var contract = Named("A", "Example.Contract");
        Set(contract, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(provider, "GetAdditionalInterfaces", ImmutableArray.Create(contract));
        Set(provider, "get_AreInterfacesComplete", true);
        Assert.Same(contract, Assert.Single(array.Interfaces));
        Assert.Same(contract, Assert.Single(array.AllInterfaces));
    }

    [Fact]
    public void PlainBaseContributesOnlyItsOwnMembersAndInterfaces()
    {
        var element = Named("A", "Example.Value");
        var baseType = Named("A", "Example.ArrayBase");
        var contract = Named("A", "Example.Contract");
        var member = Make<FieldSymbol>(SymbolKind.Field, "Length");
        Set(baseType, "get_Interfaces", ImmutableArray.Create(contract));
        Set(baseType, "get_AllInterfaces", ImmutableArray.Create(contract));
        Set(baseType, "GetMembers", ImmutableArray.Create<Symbol>(member));
        Set(baseType, "LookupType", null);
        var array = new ArrayTypeSymbolImplementation(baseType, element, null, null, null, []);
        Assert.Same(member, Assert.Single(array.GetMembers()));
        Assert.True(array.IsMemberDefined("Length", out var found));
        Assert.Same(member, found);
        Assert.False(array.IsMemberDefined("Missing", out found));
        Assert.Null(found);
        Assert.Null(array.LookupType("Missing"));
        Assert.Same(contract, Assert.Single(array.Interfaces));
        Assert.Same(contract, Assert.Single(array.AllInterfaces));
    }

    [Fact]
    public void GeneratedVisitorsDispatchAsArrayTypes()
    {
        var array = new ArrayTypeSymbolImplementation(null, Named("A", "Example.Value"), null, null, null, []);
        var visitor = new RecordingVisitor();
        array.Accept(visitor);
        Assert.Same(array, visitor.Seen);
        Assert.Equal("array", array.Accept(new ResultVisitor()));
    }

    private sealed class RecordingVisitor : SymbolVisitor
    {
        public ArrayTypeSymbol? Seen { get; private set; }
        public override void VisitArrayType(ArrayTypeSymbol symbol) => Seen = symbol;
    }

    private sealed class ResultVisitor : SymbolVisitor<string>
    {
        public override string VisitArrayType(ArrayTypeSymbol symbol) => "array";
        public override string DefaultVisit(Symbol symbol) => "default";
    }

    private static Provider CreateProvider()
    {
        var provider = Make<Provider>(SymbolKind.Type, "ArrayBase", TypeKind.Class);
        Set(provider, "get_AreInterfacesComplete", true);
        Set(provider, "get_Interfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(provider, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(provider, "GetAdditionalInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return provider;
    }

    internal interface Provider : NamedTypeSymbol, IArrayTypeProvider { }
}
