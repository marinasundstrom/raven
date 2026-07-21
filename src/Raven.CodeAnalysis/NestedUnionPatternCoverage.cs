using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class NestedUnionPatternCoverage
{
    public static void AccumulateAndRemoveCoveredCase(
        HashSet<IUnionCaseTypeSymbol> remaining,
        BoundPattern pattern,
        IUnionSymbol union,
        Dictionary<IUnionCaseTypeSymbol, List<BoundCasePattern>> patternsByCase,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        foreach (var casePattern in EnumerateCasePatterns(pattern))
        {
            if (!AreSameUnion(casePattern.CaseSymbol.Union, union))
                continue;

            var matchedCase = Normalize(casePattern.CaseSymbol);
            var entry = patternsByCase.FirstOrDefault(pair => SameCase(pair.Key, matchedCase));
            if (entry.Key is null)
            {
                entry = new KeyValuePair<IUnionCaseTypeSymbol, List<BoundCasePattern>>(
                    matchedCase,
                    []);
                patternsByCase.Add(entry.Key, entry.Value);
            }

            entry.Value.Add(casePattern);

            if (CaseArgumentsAreCollectivelyCovered(matchedCase, entry.Value, isTotalPattern))
                remaining.RemoveWhere(candidate => SameCase(candidate, matchedCase));
        }
    }

    private static bool CaseArgumentsAreCollectivelyCovered(
        IUnionCaseTypeSymbol caseSymbol,
        IReadOnlyList<BoundCasePattern> patterns,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        var parameters = caseSymbol.ConstructorParameters;
        if (parameters.Length == 0)
            return true;

        if (patterns.Any(pattern => ArgumentsAreTotal(parameters, pattern.Arguments, isTotalPattern)))
            return true;

        // Combining constrained patterns is currently supported for the common
        // single-payload case. Multiple payloads require a Cartesian pattern matrix
        // so that correlated combinations are not incorrectly treated as exhaustive.
        if (parameters.Length != 1)
            return false;

        var arguments = patterns
            .Where(pattern => pattern.Arguments.Length == 1)
            .SelectMany(pattern => ExpandOrPatterns(pattern.Arguments[0]))
            .ToImmutableArray();

        return PatternsCoverType(parameters[0].Type, arguments, isTotalPattern);
    }

    private static bool PatternsCoverType(
        ITypeSymbol type,
        ImmutableArray<BoundPattern> patterns,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        if (patterns.Any(pattern => isTotalPattern(type, pattern)))
            return true;

        var union = type.TryGetUnion() ?? type.TryGetUnionCase()?.Union;
        if (union is null || union.DeclaredCaseTypes.IsDefaultOrEmpty)
            return false;

        foreach (var nestedCase in union.DeclaredCaseTypes)
        {
            var matchingPatterns = patterns
                .OfType<BoundCasePattern>()
                .Where(pattern =>
                    AreSameUnion(pattern.CaseSymbol.Union, union) &&
                    SameCase(pattern.CaseSymbol, nestedCase))
                .ToArray();

            if (matchingPatterns.Length == 0 ||
                !CaseArgumentsAreCollectivelyCovered(nestedCase, matchingPatterns, isTotalPattern))
            {
                return false;
            }
        }

        return true;
    }

    private static bool ArgumentsAreTotal(
        ImmutableArray<IParameterSymbol> parameters,
        ImmutableArray<BoundPattern> arguments,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        if (parameters.Length != arguments.Length)
            return false;

        for (var i = 0; i < parameters.Length; i++)
        {
            if (!isTotalPattern(parameters[i].Type, arguments[i]))
                return false;
        }

        return true;
    }

    private static IEnumerable<BoundCasePattern> EnumerateCasePatterns(BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundCasePattern casePattern:
                yield return casePattern;
                break;
            case BoundOrPattern orPattern:
                foreach (var nested in EnumerateCasePatterns(orPattern.Left))
                    yield return nested;
                foreach (var nested in EnumerateCasePatterns(orPattern.Right))
                    yield return nested;
                break;
        }
    }

    private static IEnumerable<BoundPattern> ExpandOrPatterns(BoundPattern pattern)
    {
        if (pattern is not BoundOrPattern orPattern)
        {
            yield return pattern;
            yield break;
        }

        foreach (var nested in ExpandOrPatterns(orPattern.Left))
            yield return nested;
        foreach (var nested in ExpandOrPatterns(orPattern.Right))
            yield return nested;
    }

    private static IUnionCaseTypeSymbol Normalize(IUnionCaseTypeSymbol caseSymbol)
        => caseSymbol.OriginalDefinition as IUnionCaseTypeSymbol ?? caseSymbol;

    private static bool SameCase(IUnionCaseTypeSymbol left, IUnionCaseTypeSymbol right)
    {
        left = Normalize(left);
        right = Normalize(right);
        return left.Ordinal == right.Ordinal && AreSameUnion(left.Union, right.Union);
    }

    private static bool AreSameUnion(IUnionSymbol left, IUnionSymbol right)
        => SymbolEqualityComparer.Default.Equals(
            UnwrapAlias((ITypeSymbol)left),
            UnwrapAlias((ITypeSymbol)right));

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            type = alias;

        return type;
    }
}
