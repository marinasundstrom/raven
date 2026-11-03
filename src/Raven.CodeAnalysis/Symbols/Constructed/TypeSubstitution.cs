using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal interface IConstructedTypeSubstitutionInfo
{
    NamedTypeSymbol DefinitionForSubstitution { get; }
    ImmutableArray<TypeSymbol> ExplicitTypeArgumentsForSubstitution { get; }
}

internal static class TypeSubstitution
{
    internal static TypeSymbol SubstituteStructuralUnion(
        StructuralUnionTypeSymbol union, Func<TypeSymbol, TypeSymbol> substitute)
    {
        var types = union.ConstituentTypes.Select(substitute).ToImmutableArray();
        return types.Where((type, index) => !ReferenceEquals(type, union.ConstituentTypes[index])).Any()
            ? StructuralUnionTypeSymbolImplementation.Create(types)
            : union;
    }

    internal static TypeSymbol SubstituteIntersection(
        IntersectionTypeSymbol intersection, Func<TypeSymbol, TypeSymbol> substitute)
    {
        var types = intersection.ConstituentTypes.Select(substitute).ToImmutableArray();
        return types.Where((type, index) => !ReferenceEquals(type, intersection.ConstituentTypes[index])).Any()
            ? IntersectionTypeSymbolImplementation.Create(types)
            : intersection;
    }

    internal static NamedTypeSymbol GetDefinitionForSubstitution(NamedTypeSymbol type)
    {
        if (type is IConstructedTypeSubstitutionInfo constructed)
            return constructed.DefinitionForSubstitution;

        return (NamedTypeSymbol?)(type.ConstructedFrom ?? type) ?? type;
    }

    internal static ImmutableArray<TypeSymbol> GetShallowTypeArguments(NamedTypeSymbol type)
    {
        if (type is IConstructedTypeSubstitutionInfo constructed)
            return constructed.ExplicitTypeArgumentsForSubstitution;

        if ((type.TypeArguments.IsDefaultOrEmpty || type.TypeArguments.Length == 0) &&
            !type.TypeParameters.IsDefaultOrEmpty &&
            type.TypeParameters.Length > 0)
        {
            return type.TypeParameters.Cast<TypeSymbol>().ToImmutableArray();
        }

        return type.TypeArguments;
    }

    internal static TypeSymbol SubstituteTupleElements(
        TupleTypeSymbol tuple,
        Func<TypeSymbol, TypeSymbol> substitute)
    {
        var elements = tuple.TupleElements;
        if (elements.IsDefaultOrEmpty)
            return tuple;

        var substitutedElements = new TypeSymbol[elements.Length];
        var changed = false;
        for (var i = 0; i < elements.Length; i++)
        {
            substitutedElements[i] = substitute(elements[i].Type);
            changed |= !ReferenceEquals(substitutedElements[i], elements[i].Type);
        }

        if (!changed)
            return tuple;

        if (tuple is StructuralTupleTypeSymbol)
            return new StructuralTupleTypeSymbol(elements.Select((element, index) => ((string?)element.Name, substitutedElements[index])));

        var underlyingDefinition = GetDefinitionForSubstitution(tuple.UnderlyingTupleType!);
        if (underlyingDefinition.Arity != substitutedElements.Length)
            return tuple;

        var underlying = underlyingDefinition.Construct(substitutedElements) as NamedTypeSymbol;
        if (underlying is null)
            return tuple;

        var substitutedTuple = new TupleTypeSymbolImplementation(
            underlying,
            tuple.ContainingSymbol!,
            tuple.ContainingType,
            tuple.ContainingNamespace,
            []);
        var underlyingFields = new FieldSymbol[elements.Length];
        for (var i = 0; i < underlyingFields.Length; i++)
        {
            var metadataName = $"Item{i + 1}";
            var underlyingField = underlying.GetMembers(metadataName)
                .OfType<FieldSymbol>()
                .FirstOrDefault(field => !field.IsStatic);
            if (underlyingField is null)
                return tuple;

            underlyingFields[i] = underlyingField;
        }

        substitutedTuple.SetTupleElements(elements.Select((element, index) =>
            new TupleFieldSymbol(
                element.Name,
                underlyingFields[index],
                underlying,
                [],
                substitutedElements[index])));
        return substitutedTuple;
    }

    internal static bool TryGetEquivalentTypeParameterSubstitution(
        TypeParameterSymbol parameter,
        IReadOnlyDictionary<TypeParameterSymbol, TypeSymbol> substitutions,
        out TypeSymbol replacement)
    {
        foreach (var entry in substitutions)
        {
            if (!AreEquivalentTypeParameters(parameter, entry.Key))
                continue;

            replacement = entry.Value;
            return true;
        }

        replacement = null!;
        return false;
    }

    internal static bool AreEquivalentTypeParameters(
        TypeParameterSymbol left,
        TypeParameterSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left.OwnerKind != right.OwnerKind ||
            left.Ordinal != right.Ordinal)
        {
            return false;
        }

        return HaveEquivalentTypeParameterOwners(left.ContainingSymbol, right.ContainingSymbol);
    }

    private static bool HaveEquivalentTypeParameterOwners(
        Symbol? leftOwner,
        Symbol? rightOwner)
    {
        if (leftOwner is null || rightOwner is null)
            return false;

        if (SymbolEqualityComparer.Default.Equals(leftOwner, rightOwner))
            return true;

        if (leftOwner is NamedTypeSymbol leftType &&
            rightOwner is NamedTypeSymbol rightType)
        {
            return SymbolEqualityComparer.Default.Equals(
                GetDefinitionForSubstitution(leftType),
                GetDefinitionForSubstitution(rightType));
        }

        if (leftOwner is MethodSymbol leftMethod &&
            rightOwner is MethodSymbol rightMethod)
        {
            return SymbolEqualityComparer.Default.Equals(
                leftMethod.OriginalDefinition ?? leftMethod,
                rightMethod.OriginalDefinition ?? rightMethod);
        }

        return false;
    }

    internal static NamedTypeSymbol ReanchorNested(
        NamedTypeSymbol nestedDefinition,
        NamedTypeSymbol containingOverride,
        Dictionary<TypeParameterSymbol, TypeSymbol>? inheritedSubstitution,
        ImmutableArray<TypeSymbol> typeArguments)
    {
        return ConstructedNamedTypeSymbol.ReanchorNested(
            nestedDefinition,
            containingOverride,
            inheritedSubstitution,
            typeArguments);
    }

    internal static void AddContainingTypeSubstitutions(
        NamedTypeSymbol? containingType,
        Dictionary<TypeParameterSymbol, TypeSymbol> substitutionMap)
    {
        if (containingType is null)
            return;

        if (containingType.ContainingType is NamedTypeSymbol outer)
            AddContainingTypeSubstitutions(outer, substitutionMap);

        var definition = GetDefinitionForSubstitution(containingType);
        var typeParameters = definition.TypeParameters;
        if (typeParameters.IsDefaultOrEmpty || typeParameters.Length == 0)
            return;

        var typeArguments = GetShallowTypeArguments(containingType);
        if (typeArguments.IsDefaultOrEmpty || typeArguments.Length == 0)
            return;

        var arity = Math.Min(typeParameters.Length, typeArguments.Length);
        for (var i = 0; i < arity; i++)
        {
            var key = (TypeParameterSymbol)(typeParameters[i].OriginalDefinition ?? typeParameters[i]);
            var value = typeArguments[i];

            if (!substitutionMap.TryGetValue(key, out var existing) || existing is TypeParameterSymbol)
                substitutionMap[key] = value;
        }
    }
}
