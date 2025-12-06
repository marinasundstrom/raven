using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class TypeCoverageHelper
{
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
