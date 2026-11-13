using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "NeoCLR")]
public sealed class StructuralTupleContractTests
{
    [Fact]
    public void NamedElementsExposeStorageAliasesWithoutDuplicatingDefaultNames()
    {
        var type = Named("Types", "Example.Element");
        var tuple = new StructuralTupleTypeSymbol([("value", type), (null, type), ("Item3", type)]);
        Assert.Equal(new[] { "value", "Item2", "Item3" }, tuple.TupleElements.Select(field => field.Name));
        Assert.Equal(new[] { "value", "Item1", "Item2", "Item3" }, tuple.GetMembers().Select(member => member.Name));
        Assert.True(tuple.IsMemberDefined("value", out var named));
        Assert.Same(tuple.TupleElements[0], named);
        Assert.True(tuple.IsMemberDefined("Item1", out var alias));
        Assert.NotSame(named, alias);
        Assert.Equal("Item1", named!.MetadataName);
        Assert.Same(type, Assert.IsAssignableFrom<FieldSymbol>(alias).Type);
        Assert.False(tuple.IsMemberDefined("Missing", out var missing));
        Assert.Null(missing);
        Assert.Empty(tuple.GetMembers("Missing"));
        Assert.Null(tuple.LookupType("Nested"));
        Assert.All(tuple.GetMembers(), member =>
        {
            var field = Assert.IsAssignableFrom<FieldSymbol>(member);
            Assert.Same(tuple, field.ContainingSymbol);
            Assert.Same(tuple, field.ContainingType);
            Assert.Same(type, field.Type);
            Assert.True(field.IsImplicitlyDeclared);
            Assert.Equal(Accessibility.Public, field.DeclaredAccessibility);
            Assert.Equal(RefKind.None, field.RefKind);
            Assert.False(field.IsReadOnly);
            Assert.False(field.IsStatic);
            Assert.False(field.IsConst);
            Assert.False(field.IsRequired);
            Assert.Null(field.GetConstantValue());
            Assert.Equal("field", field.Accept(new Visitor()));
        });
    }

    [Fact]
    public void NativeShapeHasNoNominalCarrierAndSupportsEmptyTuple()
    {
        var tuple = new StructuralTupleTypeSymbol([]);
        Assert.Equal("Tuple", tuple.Name);
        Assert.Equal("", tuple.MetadataName);
        Assert.Equal("()", tuple.ToString());
        Assert.Equal(TypeKind.Tuple, tuple.TypeKind);
        Assert.True(tuple.IsValueType);
        Assert.True(tuple.IsClosed);
        Assert.True(tuple.IsImplicitlyDeclared);
        Assert.False(tuple.IsAbstract);
        Assert.False(tuple.IsGenericType);
        Assert.False(tuple.IsUnboundGenericType);
        Assert.Null(tuple.UnderlyingTupleType);
        Assert.Null(tuple.BaseType);
        Assert.Null(tuple.OriginalDefinition);
        Assert.Null(tuple.ConstructedFrom);
        Assert.Null(tuple.ContainingAssembly);
        Assert.Null(tuple.StaticConstructor);
        Assert.Empty(tuple.TypeArguments);
        Assert.Empty(tuple.TypeParameters);
        Assert.Empty(tuple.TupleElements);
        Assert.Empty(tuple.GetMembers());
        Assert.Empty(tuple.Interfaces);
        Assert.Empty(tuple.AllInterfaces);
        Assert.Empty(tuple.Constructors);
        Assert.Empty(tuple.InstanceConstructors);
        Assert.Same(tuple, tuple.Construct());
        Assert.Equal("tuple", tuple.Accept(new Visitor()));
    }

    private sealed class Visitor : SymbolVisitor<string>
    {
        public override string VisitNamedType(NamedTypeSymbol symbol) => "tuple";
        public override string VisitField(FieldSymbol symbol) => "field";
    }
}
