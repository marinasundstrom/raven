using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class PointerTypeContractTests
{
    [Fact]
    public void PointerShapePreservesElementIdentity()
    {
        var element = Named("A", "Example.Value");
        var pointer = new PointerTypeSymbolImplementation(element);
        Assert.Same(element, pointer.PointedAtType);
        Assert.Same(element, pointer.OriginalDefinition);
        Assert.Same(element, pointer.ContainingSymbol);
        Assert.Equal("Value*", pointer.Name);
        Assert.Equal("Value*", pointer.MetadataName);
        Assert.Equal(pointer.Name, pointer.ToString());
        Assert.Equal(SymbolKind.Type, pointer.Kind);
        Assert.Equal(TypeKind.Pointer, pointer.TypeKind);
        Assert.Equal(SpecialType.None, pointer.SpecialType);
        Assert.True(pointer.IsType);
        Assert.True(pointer.IsImplicitlyDeclared);
        Assert.False(pointer.IsNamespace);
        Assert.False(pointer.IsReferenceType);
        Assert.False(pointer.IsValueType);
    }

    [Fact]
    public void MissingElementIsRejectedBeforeReadingItsState()
    {
        Assert.Equal("pointedAtType", Assert.Throws<ArgumentNullException>(() => new PointerTypeSymbolImplementation(null!)).ParamName);
    }

    [Fact]
    public void OwnerMetadataRemainsNullable()
    {
        var element = Named("A", "Example.Value");
        Set(element, "get_ContainingAssembly", null);
        Set(element, "get_ContainingModule", null);
        var pointer = new PointerTypeSymbolImplementation(element);
        Assert.Null(pointer.ContainingAssembly);
        Assert.Null(pointer.ContainingModule);
        var context = new NullabilityInfoContext();
        Assert.Equal(NullabilityState.Nullable, context.Create(typeof(PointerTypeSymbolImplementation).GetProperty(nameof(pointer.ContainingAssembly))!).ReadState);
        Assert.Equal(NullabilityState.Nullable, context.Create(typeof(PointerTypeSymbolImplementation).GetProperty(nameof(pointer.ContainingModule))!).ReadState);
    }

    [Fact]
    public void MembersAndInterfacesDelegateToElement()
    {
        var element = Named("A", "Example.Value");
        var parent = Named("A", "Example.Parent");
        var contract = Named("A", "Example.Contract");
        var member = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(element, "get_BaseType", parent);
        Set(element, "get_Interfaces", ImmutableArray.Create(contract));
        Set(element, "get_AllInterfaces", ImmutableArray.Create(contract));
        Set(element, "GetMembers", ImmutableArray.Create<Symbol>(member));
        var pointer = new PointerTypeSymbolImplementation(element);
        Assert.Same(parent, pointer.BaseType);
        Assert.Same(contract, Assert.Single(pointer.Interfaces));
        Assert.Same(contract, Assert.Single(pointer.AllInterfaces));
        Assert.Same(member, Assert.Single(pointer.GetMembers()));
        Assert.Same(member, Assert.Single(pointer.GetMembers("Value")));
        Assert.Null(pointer.LookupType("Value"));
        Assert.False(pointer.IsMemberDefined("Value", out var found));
        Assert.Null(found);
    }

    [Fact]
    public void EqualityAndHashingUseElementSymbolIdentity()
    {
        var element = Named("A", "Example.Value");
        Set(element, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(element, "get_Arity", 0);
        var first = new PointerTypeSymbolImplementation(element);
        var second = new PointerTypeSymbolImplementation(element);
        Assert.True(first.Equals((object)second));
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.False(first.Equals((object?)null));
        Assert.False(first.Equals((object)element));
    }
}
