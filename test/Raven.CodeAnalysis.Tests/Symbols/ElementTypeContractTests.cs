using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class ElementTypeContractTests
{
    [Fact]
    public void ArrayContractPreservesImmediateElementIdentity()
    {
        var element = Named("Runtime", "Example.Element");
        var array = Make<ArrayTypeSymbol>(SymbolKind.Type, "array", TypeKind.Array);
        Set(array, "get_ElementType", element);
        Assert.Same(element, TypeSymbolExtensions2.GetElementType(array));
    }

    [Fact]
    public void PointerContractPreservesImmediateElementIdentity()
    {
        var element = Named("Runtime", "Example.Element");
        var pointer = Make<PointerTypeSymbol>(SymbolKind.Type, "pointer");
        Set(pointer, "get_PointedAtType", element);
        Assert.Same(element, TypeSymbolExtensions2.GetElementType(pointer));
    }

    [Fact]
    public void AddressContractPreservesImmediateElementIdentity()
    {
        var element = Named("Runtime", "Example.Element");
        var address = Make<AddressTypeSymbol>(SymbolKind.Type, "address");
        Set(address, "get_ReferencedType", element);
        Assert.Same(element, TypeSymbolExtensions2.GetElementType(address));
    }

    [Fact]
    public void ByRefRepresentationDoesNotRequireConcreteStorage()
    {
        var element = Named("Runtime", "Example.Element");
        var reference = Make<ByRefType>(SymbolKind.Type, "reference");
        Set(reference, "get_ElementType", element);
        Assert.Same(element, TypeSymbolExtensions2.GetElementType(reference));
    }

    [Fact]
    public void ConcreteByRefRetainsItsElement()
    {
        var element = Named("Runtime", "Example.Element");
        Assert.Same(element, TypeSymbolExtensions2.GetElementType(new RefTypeSymbol(element)));
    }

    [Fact]
    public void NestedRepresentationsAreUnwrappedOneLayerOnly()
    {
        var element = Named("Runtime", "Example.Element");
        var inner = Make<PointerTypeSymbol>(SymbolKind.Type, "inner");
        Set(inner, "get_PointedAtType", element);
        var outer = Make<ArrayTypeSymbol>(SymbolKind.Type, "outer", TypeKind.Array);
        Set(outer, "get_ElementType", inner);
        Assert.Same(inner, TypeSymbolExtensions2.GetElementType(outer));
        Assert.Same(element, TypeSymbolExtensions2.GetElementType(inner));
    }

    [Fact]
    public void NullAndOrdinaryTypesHaveNoElement()
    {
        Assert.Null(TypeSymbolExtensions2.GetElementType(null));
        Assert.Null(TypeSymbolExtensions2.GetElementType(Named("Runtime", "Example.Value")));
    }

    internal interface ByRefType : TypeSymbol, IByRefTypeInfo { }
}
