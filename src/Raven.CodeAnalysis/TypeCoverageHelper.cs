using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class TypeCoverageHelper
{
    /// <summary>
    /// Determines whether the given type participates in a sealed class hierarchy.
    /// If so, returns the <see cref="NamedTypeSymbol"/> root of the hierarchy (unwrapped to its original definition).
    /// </summary>
    public static bool TryGetSealedHierarchy(TypeSymbol scrutineeType, [NotNullWhen(true)] out NamedTypeSymbol? sealedRoot)
        => TryGetSealedHierarchy(scrutineeType, out sealedRoot, out _);

    public static bool TryGetSealedHierarchy(
        TypeSymbol scrutineeType,
        [NotNullWhen(true)] out NamedTypeSymbol? sealedRoot,
        [NotNullWhen(true)] out NamedTypeSymbol? projectedHierarchy)
        => TryGetSealedHierarchy(
            scrutineeType,
            new HashSet<TypeParameterSymbol>(SymbolEqualityComparer.Default),
            out sealedRoot,
            out projectedHierarchy);

    private static bool TryGetSealedHierarchy(
        TypeSymbol scrutineeType,
        HashSet<TypeParameterSymbol> visited,
        [NotNullWhen(true)] out NamedTypeSymbol? sealedRoot,
        [NotNullWhen(true)] out NamedTypeSymbol? projectedHierarchy)
    {
        sealedRoot = null;
        projectedHierarchy = null;

        var resolved = UnwrapAlias(scrutineeType);

        if (resolved is TypeParameterSymbol typeParameter)
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

        if (resolved is not NamedTypeSymbol named)
            return false;

        var original = named.OriginalDefinition as NamedTypeSymbol ?? named;

        if (original.IsSealedHierarchy || !original.PermittedDirectSubtypes.IsDefaultOrEmpty)
        {
            sealedRoot = original;
            projectedHierarchy = named;
            return true;
        }

        return false;
    }

    public static bool CanTypeParameterMatchPattern(TypeSymbol inputType, TypeSymbol patternType)
    {
        var input = UnwrapAlias(inputType);
        var pattern = UnwrapAlias(patternType);
        if (input is null || pattern is null)
            return false;

        return input is TypeParameterSymbol inputParameter && pattern.SatisfiesConstraints(inputParameter) ||
            pattern is TypeParameterSymbol patternParameter && input.SatisfiesConstraints(patternParameter);
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
        var pending = new Stack<NamedTypeSymbol>();
        var visited = new HashSet<NamedTypeSymbol>(ReferenceEqualityComparer.Instance);
        pending.Push(sealedType);
        while (pending.TryPop(out var current))
        {
            if (!visited.Add(current))
                continue;
            var children = current.PermittedDirectSubtypes;
            if (children.IsDefaultOrEmpty)
                continue;
            foreach (var child in children)
            {
                if (child is null)
                    continue;
                if (child.IsSealedHierarchy)
                {
                    pending.Push(child);
                    if (child.IsAbstract)
                        continue;
                }
                results.Add(child);
            }
        }
    }

    public static bool LiteralBelongsToType(ILiteralTypeInfo literal, TypeSymbol targetType)
    {
        var target = UnwrapAlias(targetType);
        var underlying = UnwrapAlias(literal.UnderlyingType);
        if (target is null || underlying is null)
            return false;

        return SymbolEqualityComparer.Default.Equals(target, underlying) ||
            target.SpecialType == SpecialType.System_Boolean && underlying.SpecialType == SpecialType.System_Boolean && literal.ConstantValue is bool ||
            target.TypeKind == TypeKind.Null && literal.ConstantValue is null;
    }

    public static bool LiteralsCoverType(TypeSymbol targetType, IEnumerable<object?> constants)
    {
        if (!RequiresLiteralCoverage(targetType))
            return false;

        var seenTrue = false;
        var seenFalse = false;
        foreach (var constant in constants)
        {
            if (constant is not bool value)
                continue;

            seenTrue |= value;
            seenFalse |= !value;
            if (seenTrue && seenFalse)
                return true;
        }

        return false;
    }

    public static bool RequiresLiteralCoverage(TypeSymbol targetType)
        => UnwrapAlias(targetType)?.SpecialType == SpecialType.System_Boolean;

    private static TypeSymbol? UnwrapAlias(TypeSymbol type)
    {
        HashSet<TypeSymbol>? visited = null;
        while (type.IsAlias)
        {
            visited ??= new(ReferenceEqualityComparer.Instance);
            if (!visited.Add(type))
                return null;
            if (type.UnderlyingSymbol is not TypeSymbol underlying)
                break;
            type = underlying;
        }

        return type;
    }
}
