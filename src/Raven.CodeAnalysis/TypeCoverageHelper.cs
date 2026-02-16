using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class TypeCoverageHelper
{
    /// <summary>
    /// Determines whether the given type participates in a sealed class hierarchy.
    /// If so, returns the <see cref="INamedTypeSymbol"/> root of the hierarchy (unwrapped to its original definition).
    /// </summary>
    public static bool TryGetSealedHierarchy(ITypeSymbol scrutineeType, out INamedTypeSymbol sealedRoot)
    {
        sealedRoot = null!;

        if (scrutineeType is not INamedTypeSymbol named)
            return false;

        var original = named.OriginalDefinition as INamedTypeSymbol ?? named;

        if (original.IsSealedHierarchy || !original.PermittedDirectSubtypes.IsDefaultOrEmpty && original.PermittedDirectSubtypes.Length > 0)
        {
            sealedRoot = original;
            return true;
        }

        return false;
    }

    /// <summary>
    /// Returns the set of concrete (non-abstract) leaf types that must be covered to achieve exhaustiveness
    /// for a sealed hierarchy rooted at <paramref name="sealedRoot"/>.
    /// </summary>
    public static ImmutableArray<INamedTypeSymbol> GetSealedHierarchyLeafTypes(INamedTypeSymbol sealedRoot)
    {
        var results = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        CollectConcreteLeafSubtypes(sealedRoot, results);

        // If the root itself is concrete (non-abstract), it is also a possible runtime type.
        if (!sealedRoot.IsAbstract)
            results.Add(sealedRoot);

        return results.ToImmutableArray();
    }

    /// <summary>
    /// Recursively collects concrete (non-abstract) leaf subtypes from a sealed hierarchy.
    /// Traverses nested sealed hierarchies to find all terminal concrete types.
    /// </summary>
    public static void CollectConcreteLeafSubtypes(
        INamedTypeSymbol sealedType,
        HashSet<INamedTypeSymbol> results)
    {
        foreach (var subtype in sealedType.PermittedDirectSubtypes)
        {
            if (subtype is INamedTypeSymbol namedSubtype &&
                (namedSubtype.IsSealedHierarchy || !namedSubtype.PermittedDirectSubtypes.IsDefaultOrEmpty && namedSubtype.PermittedDirectSubtypes.Length > 0))
            {
                // Recurse into nested sealed hierarchies.
                CollectConcreteLeafSubtypes(namedSubtype, results);

                // Only add concrete runtime types.
                if (!namedSubtype.IsAbstract)
                    results.Add(namedSubtype);
            }
            else if (subtype.IsAbstract)
            {
                // Abstract non-sealed-hierarchy: not a leaf, skip
            }
            else if (subtype is INamedTypeSymbol concreteSubtype)
            {
                results.Add(concreteSubtype);
            }
        }
    }

    public static bool LiteralBelongsToType(LiteralTypeSymbol literal, ITypeSymbol targetType)
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

    public static bool LiteralsCoverType(ITypeSymbol targetType, IEnumerable<object?> constants)
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

    public static bool RequiresLiteralCoverage(ITypeSymbol targetType)
    {
        targetType = UnwrapAlias(targetType);

        return targetType.SpecialType == SpecialType.System_Boolean;
    }

    public static bool UnionIsCoveredByTypes(ITypeUnionSymbol union, IEnumerable<ITypeSymbol> coveringTypes)
    {
        var remaining = new HashSet<ITypeSymbol>(GetUnionMembers(union), SymbolEqualityComparer.Default);
        var literalBuckets = new Dictionary<ITypeSymbol, HashSet<object?>>(SymbolEqualityComparer.Default);

        foreach (var coveringType in coveringTypes)
        {
            var normalized = UnwrapAlias(coveringType);

            if (normalized is LiteralTypeSymbol literal)
            {
                var underlying = UnwrapAlias(literal.UnderlyingType);

                if (!LiteralBelongsToType(literal, underlying))
                    continue;

                if (!literalBuckets.TryGetValue(underlying, out var set))
                {
                    set = new HashSet<object?>();
                    literalBuckets[underlying] = set;
                }

                set.Add(literal.ConstantValue);
                continue;
            }

            remaining.RemoveWhere(member => SymbolEqualityComparer.Default.Equals(member, normalized));
        }

        foreach (var (underlying, constants) in literalBuckets)
        {
            if (!remaining.Contains(underlying))
                continue;

            if (LiteralsCoverType(underlying, constants))
                remaining.Remove(underlying);
        }

        return remaining.Count == 0;
    }

    private static IEnumerable<ITypeSymbol> GetUnionMembers(ITypeUnionSymbol union)
    {
        foreach (var member in union.Types)
        {
            var normalized = UnwrapAlias(member);

            if (normalized is ITypeUnionSymbol nested)
            {
                foreach (var nestedMember in GetUnionMembers(nested))
                    yield return nestedMember;
            }
            else
            {
                yield return normalized;
            }
        }
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
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
