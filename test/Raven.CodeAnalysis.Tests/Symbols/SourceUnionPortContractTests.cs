using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceUnionPortContractTests
{
    [Theory]
    [InlineData("IsReadOnly")]
    [InlineData("HasMutableMemberShape")]
    [InlineData("BaseType")]
    public void NamedTypeExtensionPointsRemainVirtual(string name)
    {
        var property = typeof(SourceNamedTypeSymbol).GetProperty(name,
            System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.NonPublic)!;
        Assert.True(property.GetMethod!.IsVirtual);
        Assert.False(property.GetMethod.IsFinal);
    }

    [Fact]
    public void NonGenericCasesUseUnionAsMetadataContainer()
    {
        var union = Create();
        Assert.Same(union, union.GetMetadataCaseContainer());
        Assert.Null(union.CompanionType);
        Assert.True(((SourceNamedTypeIdentity)union).HasMutableMemberShape);
        Assert.True(union.IsReadOnly);
        Assert.False(Create(TypeKind.Class).IsReadOnly);
    }

    [Fact]
    public void GenericCasesUseOneNonGenericStaticCompanion()
    {
        var union = Create();
        union.SetTypeParameters([Parameter("T", 0)]);
        var companion = Assert.IsType<SynthesizedUnionCompanionTypeSymbol>(union.GetMetadataCaseContainer());
        Assert.Same(companion, union.GetMetadataCaseContainer());
        Assert.Same(union, companion.Union);
        Assert.Same(union.ContainingNamespace, companion.ContainingNamespace);
        Assert.Equal(union.Name, companion.MetadataName);
        Assert.False(companion.IsGenericType);
        Assert.True(companion.IsStatic);
        Assert.True(companion.IsAbstract);
        Assert.True(companion.IsClosed);
        Assert.Equal(Accessibility.Public, companion.DeclaredAccessibility);
    }

    [Fact]
    public void ContentNullabilityCombinesExplicitFlagAndMembers()
    {
        var union = Create();
        var member = Named("Test", "Member");
        Set(member, "get_IsNullable", false);
        union.SetVariants([member]);
        union.SetMemberTypes([member]);
        Assert.Same(member, Assert.Single(union.Variants));
        Assert.Same(member, Assert.Single(union.MemberTypes));
        Assert.False(union.ContentMayBeNull);
        union.SetContentMayBeNull(true);
        Assert.True(union.ContentMayBeNull);
        union.SetContentMayBeNull(false);
        union.SetMemberTypes([Make<TypeSymbol>(SymbolKind.Type, "null", TypeKind.Null)]);
        Assert.True(union.ContentMayBeNull);
        union.SetMemberTypes([]);
        Assert.False(union.ContentMayBeNull);
    }

    [Fact]
    public void CaseRetainsOwnersOrdinalAndMutableParameterSemantics()
    {
        var union = Create();
        var @case = Case(union);
        union.SetCases([@case]);
        Assert.Same(@case, Assert.Single(union.DeclaredCaseTypes));
        Assert.Same(union, @case.Union);
        Assert.Same(union, @case.MetadataContainingType);
        Assert.Equal(2, @case.Ordinal);
        Assert.True(((SourceNamedTypeIdentity)@case).HasMutableMemberShape);
        Assert.True(@case.IsReadOnly);
        var parameter = new SourceParameterSymbol("value", union, @case, @case, union.ContainingNamespace, [], [], isMutable: true);
        @case.SetConstructorParameters([parameter]);
        Assert.Same(parameter, Assert.Single(@case.ConstructorParameters));
        Assert.False(@case.IsReadOnly);
        @case.SetConstructorParameters([]);
        Assert.True(@case.IsReadOnly);
        Assert.False(Case(union, TypeKind.Class).IsReadOnly);
    }

    [Fact]
    public void CaseProjectionReplacesMappingAndPreservesMetadataArity()
    {
        var @case = Case(Create());
        var parameter = Parameter("TCase", 0);
        var projected = Parameter("TUnion", 0);
        @case.SetTypeParameters([parameter]);
        Assert.Equal("Value`1", @case.MetadataName);
        @case.SetProjectedUnionTypeParameters([(parameter, projected)]);
        Assert.True(@case.TryGetProjectedUnionTypeParameter(parameter, out var actual));
        Assert.Same(projected, actual);
        @case.SetProjectedUnionTypeParameters([]);
        Assert.False(@case.TryGetProjectedUnionTypeParameter(parameter, out actual));
        Assert.Null(actual);
    }

    [Fact]
    public void PayloadFieldsPreserveOrderAndPrimaryFieldIdentity()
    {
        var union = Create();
        SourceFieldSymbol Field(string name) => new(name, union, false, false, false, null, union, union, union.ContainingNamespace, [], []);
        var discriminator = Field("tag");
        var first = Field("first");
        var second = Field("second");
        union.SetDiscriminatorField(discriminator);
        union.SetPayloadFields([first, second]);
        Assert.Same(discriminator, union.DiscriminatorField);
        Assert.Same(first, union.PayloadField);
        Assert.Equal(new FieldSymbol[] { first, second }, union.PayloadFields);
    }

    private static SourceUnionSymbol Create(TypeKind kind = TypeKind.Struct)
    {
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, "Test");
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Example");
        Set(ns, "get_ContainingSymbol", assembly);
        Set(ns, "get_ContainingAssembly", assembly);
        Set(assembly, "GetTypeByMetadataName", Named("Test", "System.Object"));
        return new("Result", Named("Test", "System.ValueType"), kind, ns, null, ns, [], [], Accessibility.Public);
    }

    private static SourceUnionCaseTypeSymbol Case(SourceUnionSymbol union, TypeKind kind = TypeKind.Struct)
        => new("Value", 2, union, union.GetMetadataCaseContainer(), union, kind, union, union, union.ContainingNamespace, [], [], Accessibility.Public);
}
