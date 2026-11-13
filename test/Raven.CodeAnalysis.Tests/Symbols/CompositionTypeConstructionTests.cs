using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class CompositionTypeConstructionTests
{
    [Fact, Trait("Target", "Shared")]
    public void UnionFlattensDeduplicatesAndPreservesNominalAlternatives()
    {
        var first = Type("First");
        var second = Type("Second");
        Set(second, "get_BaseType", first);
        var union = Assert.IsAssignableFrom<StructuralUnionTypeSymbol>(StructuralUnionTypeSymbolImplementation.Create([first, second]));
        var flattened = Assert.IsAssignableFrom<StructuralUnionTypeSymbol>(StructuralUnionTypeSymbolImplementation.Create([union, first]));
        Assert.Equal(new[] { first, second }, flattened.ConstituentTypes);
        Assert.Same(first, StructuralUnionTypeSymbolImplementation.Create([first, first]));
        Assert.True(union.IsValueType);
        Assert.False(union.IsReferenceType);
        Assert.Equal("First | Second", union.Name);
        Assert.Equal("union", union.Accept(new Visitor()));
        Assert.Empty(union.GetMembers());
        Assert.False(union.IsMemberDefined("Missing", out var member));
        Assert.Null(member);
    }

    [Fact, Trait("Target", "Shared")]
    public void UnionRejectsMissingVoidAndUnboundAlternatives()
    {
        Assert.Throws<ArgumentNullException>(() => StructuralUnionTypeSymbolImplementation.Create(null!));
        Assert.Throws<ArgumentException>(() => StructuralUnionTypeSymbolImplementation.Create([]));
        Assert.Throws<ArgumentNullException>(() => StructuralUnionTypeSymbolImplementation.Create([null!]));
        var invalid = Type("Void");
        Set(invalid, "get_SpecialType", SpecialType.System_Void);
        Assert.Throws<ArgumentException>(() => StructuralUnionTypeSymbolImplementation.Create([invalid]));
        invalid = Type("Unbound");
        Set(invalid, "get_IsUnboundGenericType", true);
        Assert.Throws<ArgumentException>(() => StructuralUnionTypeSymbolImplementation.Create([invalid]));
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void IntersectionReducesOnlyNominalSupertypes()
    {
        var parent = Type("Parent");
        var child = Type("Child");
        Set(child, "get_BaseType", parent);
        Assert.Same(child, IntersectionTypeSymbolImplementation.Create([parent, child]));
        var face = Type("Face", TypeKind.Interface);
        Set(child, "get_AllInterfaces", ImmutableArray.Create(face));
        Assert.Same(child, IntersectionTypeSymbolImplementation.Create([face, child]));
        var other = Type("Other", TypeKind.Interface);
        var intersection = Assert.IsAssignableFrom<IntersectionTypeSymbol>(IntersectionTypeSymbolImplementation.Create([child, other]));
        Assert.Equal(new[] { child, other }, intersection.ConstituentTypes);
        Assert.Equal("Child & Other", intersection.Name);
        Assert.True(intersection.IsReferenceType);
        Assert.False(intersection.IsValueType);
        Assert.Equal("intersection", intersection.Accept(new Visitor()));
        Assert.Equal(new[] { face, other }, intersection.AllInterfaces);
        Assert.Equal(new[] { other }, intersection.Interfaces);
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void IntersectionFlattensAndTerminatesOnCyclicNominalGraphs()
    {
        var first = Type("First");
        var second = Type("Second");
        Set(first, "get_BaseType", second);
        Set(second, "get_BaseType", first);
        var intersection = Assert.IsAssignableFrom<IntersectionTypeSymbol>(IntersectionTypeSymbolImplementation.Create([first, second]));
        var flattened = Assert.IsAssignableFrom<IntersectionTypeSymbol>(IntersectionTypeSymbolImplementation.Create([intersection, first]));
        Assert.Equal(new[] { first, second }, flattened.ConstituentTypes);
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void IntersectionRejectsClosedAndValueBounds()
    {
        Assert.Throws<ArgumentNullException>(() => IntersectionTypeSymbolImplementation.Create(null!));
        Assert.Throws<ArgumentException>(() => IntersectionTypeSymbolImplementation.Create([]));
        Assert.Throws<ArgumentNullException>(() => IntersectionTypeSymbolImplementation.Create([null!]));
        var closed = Type("Closed");
        Set(closed, "get_IsClosed", true);
        Assert.Throws<ArgumentException>(() => IntersectionTypeSymbolImplementation.Create([closed]));
        Assert.Throws<ArgumentException>(() => IntersectionTypeSymbolImplementation.Create([Type("Value", TypeKind.Struct)]));
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void IntersectionMemberLookupDeduplicatesAndRejectsAmbiguousNames()
    {
        var first = Type("First", TypeKind.Interface);
        var second = Type("Second", TypeKind.Interface);
        var shared = Make<Symbol>(SymbolKind.Label, "Shared");
        var nested = Type("Nested");
        var otherNested = Type("OtherNested");
        Set(otherNested, "get_Name", "Nested");
        SetMembers(first, [shared, nested]);
        SetMembers(second, [shared, otherNested]);
        var intersection = Assert.IsAssignableFrom<IntersectionTypeSymbol>(IntersectionTypeSymbolImplementation.Create([first, second]));
        Assert.Equal(3, intersection.GetMembers().Length);
        Assert.True(intersection.IsMemberDefined("Shared", out var member));
        Assert.Same(shared, member);
        Assert.False(intersection.IsMemberDefined("Nested", out member));
        Assert.Null(member);
        Assert.Null(intersection.LookupType("Nested"));
        Assert.Null(intersection.LookupType("Missing"));
    }

    private static NamedTypeSymbol Type(string name, TypeKind kind = TypeKind.Class)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, kind);
        Set(type, "get_IsReferenceType", kind is TypeKind.Class or TypeKind.Interface);
        Set(type, "get_IsValueType", kind == TypeKind.Struct);
        Set(type, "get_IsClosed", false);
        Set(type, "get_IsUnboundGenericType", false);
        Set(type, "get_BaseType", null);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(type, "get_Interfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        SetMembers(type, []);
        return type;
    }

    private static void SetMembers(NamedTypeSymbol type, ImmutableArray<Symbol> members)
        => Set(type, "GetMembers", (Func<object?[]?, object?>)(args => args is { Length: > 0 }
            ? members.Where(item => item.Name == (string)args[0]!).ToImmutableArray() : members));

    private sealed class Visitor : SymbolVisitor<string>
    {
        public override string VisitStructuralUnionType(StructuralUnionTypeSymbol symbol) => "union";
        public override string VisitIntersectionType(IntersectionTypeSymbol symbol) => "intersection";
    }
}
