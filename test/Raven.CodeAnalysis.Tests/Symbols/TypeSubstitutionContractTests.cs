using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class TypeSubstitutionContractTests
{
    [Fact, Trait("Target", "Shared")]
    public void UnionSubstitutionPreservesIdentityOrNormalizesChangedAlternatives()
    {
        var first = Type("First");
        var second = Type("Second");
        var union = Assert.IsAssignableFrom<StructuralUnionTypeSymbol>(StructuralUnionTypeSymbolImplementation.Create([first, second]));
        Assert.Same(union, TypeSubstitution.SubstituteStructuralUnion(union, type => type));
        Assert.Same(first, TypeSubstitution.SubstituteStructuralUnion(union, _ => first));
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void IntersectionSubstitutionPreservesIdentityOrCollapsesEquivalentBounds()
    {
        var first = Type("First");
        var second = Type("Second");
        var intersection = Assert.IsAssignableFrom<IntersectionTypeSymbol>(IntersectionTypeSymbolImplementation.Create([first, second]));
        Assert.Same(intersection, TypeSubstitution.SubstituteIntersection(intersection, type => type));
        Assert.Same(first, TypeSubstitution.SubstituteIntersection(intersection, _ => first));
    }

    [Fact, Trait("Target", "NeoCLR")]
    public void StructuralTupleSubstitutionPreservesNamesAndDoesNotMutateOriginal()
    {
        var first = Type("First");
        var second = Type("Second");
        var tuple = new StructuralTupleTypeSymbol([("value", first), (null, second)]);
        Assert.Same(tuple, TypeSubstitution.SubstituteTupleElements(tuple, type => type));
        var changed = Assert.IsType<StructuralTupleTypeSymbol>(TypeSubstitution.SubstituteTupleElements(tuple, _ => second));
        Assert.Equal(new[] { "value", "Item2" }, changed.TupleElements.Select(field => field.Name));
        Assert.All(changed.TupleElements, field => Assert.Same(second, field.Type));
        Assert.Same(first, tuple.TupleElements[0].Type);
        Assert.Null(changed.UnderlyingTupleType);
        var empty = new StructuralTupleTypeSymbol([]);
        Assert.Same(empty, TypeSubstitution.SubstituteTupleElements(empty, _ => throw new InvalidOperationException()));
    }

    [Fact, Trait("Target", "Shared")]
    public void DefinitionAndArgumentsFollowOpenAndConstructedShapes()
    {
        var parameter = Parameter("T", 0);
        var definition = Type("Definition");
        Set(definition, "get_TypeParameters", ImmutableArray.Create(parameter));
        Assert.Same(definition, TypeSubstitution.GetDefinitionForSubstitution(definition));
        Assert.Equal(new TypeSymbol[] { parameter }, TypeSubstitution.GetShallowTypeArguments(definition));
        var argument = Type("Argument");
        var constructed = Type("Constructed");
        Set(constructed, "get_ConstructedFrom", definition);
        Set(constructed, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(argument));
        Assert.Same(definition, TypeSubstitution.GetDefinitionForSubstitution(constructed));
        Assert.Equal(new TypeSymbol[] { argument }, TypeSubstitution.GetShallowTypeArguments(constructed));
    }

    [Fact, Trait("Target", "Shared")]
    public void ContainingSubstitutionReplacesOnlyMissingOrParameterEntries()
    {
        var parameter = Parameter("T", 0);
        var definition = Type("Definition");
        Set(definition, "get_TypeParameters", ImmutableArray.Create(parameter));
        var value = Type("Value");
        var constructed = Type("Constructed");
        Set(constructed, "get_ConstructedFrom", definition);
        Set(constructed, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(value));
        var map = new Dictionary<TypeParameterSymbol, TypeSymbol>(ReferenceEqualityComparer.Instance);
        TypeSubstitution.AddContainingTypeSubstitutions(null, map);
        Assert.Empty(map);
        TypeSubstitution.AddContainingTypeSubstitutions(constructed, map);
        Assert.Same(value, map[parameter]);
        map[parameter] = parameter;
        TypeSubstitution.AddContainingTypeSubstitutions(constructed, map);
        Assert.Same(value, map[parameter]);
        var explicitValue = Type("Explicit");
        map[parameter] = explicitValue;
        TypeSubstitution.AddContainingTypeSubstitutions(constructed, map);
        Assert.Same(explicitValue, map[parameter]);
        Assert.True(TypeSubstitution.TryGetEquivalentTypeParameterSubstitution(parameter, map, out var replacement));
        Assert.Same(explicitValue, replacement);
        Assert.False(TypeSubstitution.TryGetEquivalentTypeParameterSubstitution(parameter, new Dictionary<TypeParameterSymbol, TypeSymbol>(), out replacement));
        Assert.Null(replacement);
    }

    [Fact, Trait("Target", "Shared")]
    public void ParameterEquivalenceUsesDefinitionOwnersAndOrdinals()
    {
        var definition = Type("Definition");
        var owner = Type("Constructed");
        Set(owner, "get_ConstructedFrom", definition);
        var left = Parameter("T", 0);
        var right = Parameter("Renamed", 0);
        Set(left, "get_ContainingSymbol", definition);
        Set(left, "get_DeclaringTypeParameterOwner", definition);
        Set(right, "get_ContainingSymbol", owner);
        Set(right, "get_DeclaringTypeParameterOwner", owner);
        Assert.True(TypeSubstitution.AreEquivalentTypeParameters(left, right));
        Set(right, "get_Ordinal", 1);
        Assert.False(TypeSubstitution.AreEquivalentTypeParameters(left, right));
        Set(right, "get_Ordinal", 0);
        Set(right, "get_ContainingSymbol", null);
        Assert.False(TypeSubstitution.AreEquivalentTypeParameters(left, right));
    }

    [Fact, Trait("Target", "DotNet")]
    public void NominalTupleSubstitutionRebuildsStorageAndPreservesLogicalName()
    {
        var first = Type("First");
        var second = Type("Second");
        NamedTypeSymbol Storage(TypeSymbol element)
        {
            var storage = Type("Storage");
            Set(storage, "get_Arity", 1);
            Set(storage, "get_SpecialType", SpecialType.System_ValueTuple_T1);
            Set(storage, "get_TypeArguments", ImmutableArray.Create(element));
            Set(storage, "Construct", (Func<object?[]?, object?>)(args => Storage(((TypeSymbol[])args![0]!)[0])));
            var field = Make<FieldSymbol>(SymbolKind.Field, "Item1");
            Set(field, "get_IsStatic", false);
            Set(field, "get_Type", element);
            Set(storage, "GetMembers", ImmutableArray.Create<Symbol>(field));
            return storage;
        }
        var storage = Storage(first);
        var tuple = TupleStorageLayout.Project(storage, TupleStorageLayout.Describe(storage), [("value", first)])!;
        Assert.Same(tuple, TypeSubstitution.SubstituteTupleElements(tuple, type => type));
        var changed = Assert.IsAssignableFrom<TupleTypeSymbol>(TypeSubstitution.SubstituteTupleElements(tuple, _ => second));
        Assert.NotSame(storage, changed.UnderlyingTupleType);
        Assert.Same(second, Assert.Single(changed.UnderlyingTupleType!.TypeArguments));
        var field = Assert.Single(changed.TupleElements);
        Assert.Equal("value", field.Name);
        Assert.Same(second, field.Type);
        Assert.Same(first, tuple.TupleElements[0].Type);
        Set(storage, "Construct", null);
        Assert.Same(tuple, TypeSubstitution.SubstituteTupleElements(tuple, _ => second));
    }

    private static NamedTypeSymbol Type(string name)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Interface);
        Set(type, "get_ConstructedFrom", null);
        Set(type, "get_IsReferenceType", true);
        Set(type, "get_IsValueType", false);
        Set(type, "get_IsClosed", false);
        Set(type, "get_IsUnboundGenericType", false);
        Set(type, "get_BaseType", null);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(type, "get_Interfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        return type;
    }
}
