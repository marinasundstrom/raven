using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class TypeCoverageHelper
{
    /// <summary>
    /// Determines whether the given type participates in a sealed class hierarchy.
    /// If so, returns the <see cref="NamedTypeSymbol"/> root of the hierarchy (unwrapped to its original definition).
    /// </summary>
    public static bool TryGetSealedHierarchy(TypeSymbol scrutineeType, out NamedTypeSymbol sealedRoot)
        => TryGetSealedHierarchy(scrutineeType, out sealedRoot, out _);

    public static bool TryGetSealedHierarchy(
        TypeSymbol scrutineeType,
        out NamedTypeSymbol sealedRoot,
        out NamedTypeSymbol projectedHierarchy)
        => TryGetSealedHierarchy(
            scrutineeType,
            new HashSet<TypeParameterSymbol>(SymbolEqualityComparer.Default),
            out sealedRoot,
            out projectedHierarchy);

    private static bool TryGetSealedHierarchy(
        TypeSymbol scrutineeType,
        HashSet<TypeParameterSymbol> visited,
        out NamedTypeSymbol sealedRoot,
        out NamedTypeSymbol projectedHierarchy)
    {
        sealedRoot = null!;
        projectedHierarchy = null!;

        scrutineeType = UnwrapAlias(scrutineeType);

        if (scrutineeType is TypeParameterSymbol typeParameter)
        {
            if (!visited.Add(typeParameter))
                return false;

            foreach (var constraintType in typeParameter.ConstraintTypes)
            {
                if (TryGetSealedHierarchy(
                        constraintType,
                        visited,
                        out sealedRoot,
                        out projectedHierarchy))
                {
                    return true;
                }
            }

            return false;
        }

        if (scrutineeType is not NamedTypeSymbol named)
            return false;

        var original = named.OriginalDefinition as NamedTypeSymbol ?? named;

        if (original.IsSealedHierarchy || !original.PermittedDirectSubtypes.IsDefaultOrEmpty && original.PermittedDirectSubtypes.Length > 0)
        {
            sealedRoot = original;
            projectedHierarchy = named;
            return true;
        }

        return false;
    }

    public static bool CanTypeParameterMatchPattern(TypeSymbol inputType, TypeSymbol patternType)
    {
        inputType = UnwrapAlias(inputType);
        patternType = UnwrapAlias(patternType);

        return inputType is TypeParameterSymbol inputTypeParameter &&
                patternType.SatisfiesConstraints(inputTypeParameter) ||
            patternType is TypeParameterSymbol patternTypeParameter &&
                inputType.SatisfiesConstraints(patternTypeParameter);
    }

    /// <summary>
    /// Returns the set of types that must be covered to achieve exhaustiveness for a sealed hierarchy.
    /// Closed branches contribute concrete leaf types. Open intermediate branches contribute the branch type itself.
    /// </summary>
    public static ImmutableArray<NamedTypeSymbol> GetSealedHierarchyCoverageTypes(
        NamedTypeSymbol sealedRoot,
        NamedTypeSymbol? projectedHierarchy = null)
    {
        var results = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);
        CollectCoverageSubtypes(sealedRoot, results);

        // If the root itself is concrete (non-abstract), it is also a possible runtime type.
        if (!sealedRoot.IsAbstract)
            results.Add(sealedRoot);

        if (projectedHierarchy is null)
            return results.ToImmutableArray();

        var projectedResults = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var result in results)
            projectedResults.Add(SealedHierarchyFacts.ProjectCaseTypeToHierarchyArguments(result, projectedHierarchy));

        return projectedResults.ToImmutableArray();
    }

    /// <summary>
    /// Recursively collects required coverage types from a sealed hierarchy.
    /// Traverses only through sealed intermediate nodes; open intermediates become required coverage nodes.
    /// </summary>
    public static void CollectCoverageSubtypes(
        NamedTypeSymbol sealedType,
        HashSet<NamedTypeSymbol> results)
    {
        foreach (var subtype in sealedType.PermittedDirectSubtypes)
        {
            if (subtype is not NamedTypeSymbol namedSubtype)
                continue;

            if (namedSubtype.IsSealedHierarchy)
            {
                // Recurse into nested sealed hierarchies.
                CollectCoverageSubtypes(namedSubtype, results);

                // Only add concrete runtime types.
                if (!namedSubtype.IsAbstract)
                    results.Add(namedSubtype);
            }
            else
            {
                // Open hierarchy branch: require covering the branch node itself.
                results.Add(namedSubtype);
            }
        }
    }

    public static bool LiteralBelongsToType(LiteralTypeSymbol literal, TypeSymbol targetType)
    {
        targetType = UnwrapAlias(targetType);
        var literalUnderlying = UnwrapAlias(literal.UnderlyingType);

        if (SymbolEqualityComparer.Default.Equals(targetType, literalUnderlying))
            return true;

        if (targetType.SpecialType == SpecialType.System_Boolean &&
            literalUnderlying.SpecialType == SpecialType.System_Boolean &&
            literal.ConstantValue is bool)
        {
            return true;
        }

        if (targetType.TypeKind == TypeKind.Null && literal.ConstantValue is null)
            return true;

        return false;
    }

    public static bool LiteralsCoverType(TypeSymbol targetType, IEnumerable<object?> constants)
    {
        targetType = UnwrapAlias(targetType);

        if (targetType.SpecialType == SpecialType.System_Boolean)
        {
            var coverage = BooleanLiteralCoverage.None;

            foreach (var constant in constants)
            {
                if (constant is not bool value)
                    continue;

                coverage |= value ? BooleanLiteralCoverage.True : BooleanLiteralCoverage.False;

                if (coverage == BooleanLiteralCoverage.All)
                    return true;
            }

            return coverage == BooleanLiteralCoverage.All;
        }

        return false;
    }

    public static bool RequiresLiteralCoverage(TypeSymbol targetType)
    {
        targetType = UnwrapAlias(targetType);

        return targetType.SpecialType == SpecialType.System_Boolean;
    }

    private static TypeSymbol UnwrapAlias(TypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is TypeSymbol alias)
            type = alias;

        return type;
    }

    [System.Flags]
    private enum BooleanLiteralCoverage : byte
    {
        None = 0,
        True = 1,
        False = 2,
        All = True | False,
    }
}
