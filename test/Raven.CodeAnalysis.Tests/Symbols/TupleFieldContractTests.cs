using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class TupleFieldContractTests
{
    [Fact]
    public void ProjectionPreservesMetadataFieldAndStoragePath()
    {
        var type = Named("A", "Example.Value");
        var projected = Named("A", "Example.Projected");
        var owner = Named("A", "Example.Tuple");
        var underlying = CreateField(type);
        var path = ImmutableArray.Create(underlying);
        var field = new TupleFieldSymbol("Alias", underlying, owner, [], projected, path);
        Assert.Equal("Alias", field.Name);
        Assert.Equal("Item1", field.MetadataName);
        Assert.Same(projected, field.Type);
        Assert.Same(underlying, field.UnderlyingField);
        Assert.Equal(path, field.StoragePath);
        Assert.Same(owner, field.ContainingSymbol);
        Assert.Same(owner, field.ContainingType);
        Assert.Same(field, field.UnderlyingSymbol);
        Assert.Equal(SymbolKind.Field, field.Kind);
        Assert.False(field.IsImplicitlyDeclared);
        Assert.False(field.IsAlias);
    }

    [Fact]
    public void DefaultProjectionForwardsModifiersAndOptionalMetadata()
    {
        var type = Named("A", "Example.Value");
        var owner = Named("A", "Example.Tuple");
        Set(owner, "get_ContainingAssembly", null);
        Set(owner, "get_ContainingModule", null);
        var underlying = CreateField(type);
        var field = new TupleFieldSymbol("Alias", underlying, owner, []);
        Assert.Same(type, field.Type);
        Assert.True(field.StoragePath.IsDefault);
        Assert.Equal(RefKind.Ref, field.RefKind);
        Assert.True(field.IsReadOnly);
        Assert.False(field.IsConst);
        Assert.False(field.IsRequired);
        Assert.False(field.IsStatic);
        Assert.Equal(Accessibility.Public, field.DeclaredAccessibility);
        Assert.Null(field.ContainingAssembly);
        Assert.Null(field.ContainingModule);
        Assert.Same(owner.ContainingNamespace, field.ContainingNamespace);
        Assert.Null(field.GetConstantValue());
        Assert.Empty(field.GetAttributes());
        Assert.Empty(field.DeclaringSyntaxReferences);
    }

    [Fact]
    public void LocationsAreSnapshotted()
    {
        var owner = Named("A", "Example.Tuple");
        var original = Location.None;
        var locations = new[] { original };
        var field = new TupleFieldSymbol("Alias", CreateField(owner), owner, locations);
        locations[0] = null!;
        Assert.Same(original, Assert.Single(field.Locations));
    }

    [Fact]
    public void VisitorsAndSymbolEqualityUseProjectedFieldContract()
    {
        var owner = Named("A", "Example.Tuple");
        var type = Named("A", "Example.Value");
        var underlying = CreateField(type);
        var first = new TupleFieldSymbol("Alias", underlying, owner, []);
        var second = new TupleFieldSymbol("Alias", underlying, owner, []);
        Assert.True(first.Equals((Symbol)second));
        Assert.True(first.Equals(second, SymbolEqualityComparer.Default));
        var visitor = new RecordingVisitor();
        first.Accept(visitor);
        Assert.Same(first, visitor.Seen);
        Assert.Equal("Alias", first.Accept(new NameVisitor()));
    }

    private static FieldSymbol CreateField(TypeSymbol type)
    {
        var field = Make<FieldSymbol>(SymbolKind.Field, "Item1");
        Set(field, "get_Type", type);
        Set(field, "get_RefKind", RefKind.Ref);
        Set(field, "get_IsReadOnly", true);
        Set(field, "get_IsConst", false);
        Set(field, "get_IsRequired", false);
        Set(field, "get_IsStatic", false);
        Set(field, "get_DeclaredAccessibility", Accessibility.Public);
        Set(field, "GetConstantValue", null);
        Set(field, "GetAttributes", ImmutableArray<AttributeData>.Empty);
        return field;
    }

    private sealed class RecordingVisitor : SymbolVisitor
    {
        public FieldSymbol? Seen { get; private set; }
        public override void VisitField(FieldSymbol symbol) => Seen = symbol;
    }

    private sealed class NameVisitor : SymbolVisitor<string>
    {
        public override string VisitField(FieldSymbol symbol) => symbol.Name;
    }
}
