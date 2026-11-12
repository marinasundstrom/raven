using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundAddressContractTests
{
    [Fact]
    public void AddressTypePreservesReferencedTypeAndValueEquality()
    {
        var valueType = Named("A", "Example.Value");
        Set(valueType, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(valueType, "get_Arity", 0);
        var address = new AddressTypeSymbolImplementation(valueType);
        var other = new AddressTypeSymbolImplementation(valueType);
        Assert.Same(valueType, address.ReferencedType);
        Assert.Same(valueType, address.ContainingSymbol);
        Assert.Same(valueType, address.OriginalDefinition);
        Assert.Equal(TypeKind.Address, address.TypeKind);
        Assert.Equal("&Value", address.Name);
        Assert.Equal("&Value", address.MetadataName);
        Assert.True(address.Equals((object)other));
        Assert.Equal(address.GetHashCode(), other.GetHashCode());
        Assert.True(address.IsImplicitlyDeclared);
        Assert.False(address.IsValueType);
        Assert.Null(address.LookupType("Missing"));
        Assert.False(address.IsMemberDefined("Missing", out var member));
        Assert.Null(member);
    }

    [Fact]
    public void AddressContainerMetadataPreservesOptionalOwners()
    {
        var valueType = Named("A", "Example.Value");
        Set(valueType, "get_ContainingAssembly", null);
        Set(valueType, "get_ContainingModule", null);
        var address = new AddressTypeSymbolImplementation(valueType);
        Assert.Null(address.ContainingAssembly);
        Assert.Null(address.ContainingModule);
        var context = new NullabilityInfoContext();
        Assert.Equal(NullabilityState.Nullable, context.Create(typeof(AddressTypeSymbolImplementation).GetProperty(nameof(address.ContainingAssembly))!).ReadState);
        Assert.Equal(NullabilityState.Nullable, context.Create(typeof(AddressTypeSymbolImplementation).GetProperty(nameof(address.ContainingModule))!).ReadState);
    }

    [Fact]
    public void AddressMemberResolutionIsLazyAndPrefersDerivedSignatures()
    {
        var parent = Named("A", "Example.Parent");
        var child = Named("A", "Example.Child");
        var inherited = Make<FieldSymbol>(SymbolKind.Field, "Value");
        var selected = Make<FieldSymbol>(SymbolKind.Field, "Value");
        var unrelated = Make<FieldSymbol>(SymbolKind.Field, "Other");
        Set(child, "get_BaseType", parent);
        Set(parent, "get_BaseType", null);
        var address = new AddressTypeSymbolImplementation(child);
        var members = address.ResolveMembers("Value");
        // Set the member collections after constructing the lazy query.
        Set(child, "GetMembers", ImmutableArray.Create<Symbol>(unrelated, selected));
        Set(parent, "GetMembers", ImmutableArray.Create<Symbol>(inherited));
        Assert.Same(selected, Assert.Single(members));
    }

    [Fact]
    public void AddressConstructorsRejectMissingTypes()
    {
        Assert.Equal("referencedType", Assert.Throws<ArgumentNullException>(() => new AddressTypeSymbolImplementation(null!)).ParamName);
        Assert.Equal("valueType", Assert.Throws<ArgumentNullException>(() => new BoundAddressOfExpression(null, null!)).ParamName);
        Assert.Equal("storage", Assert.Throws<ArgumentException>(() => new BoundAddressOfExpression((BoundExpression)null!)).ParamName);
    }

    [Fact]
    public void AddressTraversalVisitsStorageAndItsReceiverOnce()
    {
        var valueType = Named("A", "Example.Value");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, valueType);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", valueType);
        var storage = new BoundMemberAccessExpression(receiver, field);
        var address = new BoundAddressOfExpression(storage);
        var walker = new OperandWalker();
        walker.Visit(address);
        Assert.Equal<BoundExpression>([address, storage, receiver], walker.Expressions);
        Assert.Same(receiver, address.Receiver);
        Assert.Same(storage, address.Storage);
        Assert.Same(valueType, address.ValueType);
    }

    [Fact]
    public void ReceiverOnlyAddressAndOutFlagsArePreserved()
    {
        var valueType = Named("A", "Example.Value");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, valueType);
        var address = new BoundAddressOfExpression(null, valueType, receiver, isOutDiscard: true, isUntypedOutDeclaration: true);
        var walker = new OperandWalker();
        walker.Visit(address);
        Assert.Equal<BoundExpression>([address, receiver], walker.Expressions);
        Assert.True(address.IsTypeInferredOutArgument);
        Assert.True(address.IsOutDiscard);
        Assert.True(address.IsUntypedOutDeclaration);
        Assert.Same(valueType, Assert.IsAssignableFrom<AddressTypeSymbol>(address.Type).ReferencedType);
    }

    [Fact]
    public void FixedDeclarationTraversalVisitsItsAddressInitializer()
    {
        var valueType = Named("A", "Example.Value");
        var local = Make<LocalSymbol>(SymbolKind.Local, "pointer");
        Set(local, "get_Type", valueType);
        var storage = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, valueType);
        var address = new BoundAddressOfExpression(storage);
        var declarator = new BoundVariableDeclarator(local, null, address, local);
        var statement = new BoundLocalDeclarationStatement([declarator]);
        var walker = new OperandWalker();
        walker.Visit(statement);
        Assert.Equal<BoundExpression>([address, storage], walker.Expressions);
        Assert.Same(local, declarator.Symbol);
        Assert.Same(valueType, declarator.Type);
        Assert.Same(local, statement.Symbol);
        Assert.Same(local, declarator.FixedPinnedLocal);
    }

    private sealed class OperandWalker : BoundTreeWalker
    {
        public List<BoundExpression> Expressions { get; } = [];
        public override void VisitExpression(BoundExpression node)
        {
            Expressions.Add(node);
            base.VisitExpression(node);
        }
    }
}
