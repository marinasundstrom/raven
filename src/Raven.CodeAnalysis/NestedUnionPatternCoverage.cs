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

        if (FiniteArgumentCombinationsAreCovered(parameters, patterns, isTotalPattern))
            return true;

        // Combining constrained patterns is currently supported for the common
        // single-payload case when its domain cannot be enumerated as a small finite
        // product. Multiple non-finite payloads remain conservative.
        if (parameters.Length != 1)
            return false;

        var arguments = patterns
            .Where(pattern => pattern.Arguments.Length == 1)
            .SelectMany(pattern => ExpandOrPatterns(pattern.Arguments[0]))
            .ToImmutableArray();

        return PatternsCoverType(parameters[0].Type, arguments, isTotalPattern);
    }

    private static bool FiniteArgumentCombinationsAreCovered(
        ImmutableArray<IParameterSymbol> parameters,
        IReadOnlyList<BoundCasePattern> patterns,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        var domains = new ImmutableArray<FiniteValue>[parameters.Length];
        var combinationCount = 1;

        for (var i = 0; i < parameters.Length; i++)
        {
            if (!TryEnumerateFiniteValues(parameters[i].Type, depth: 0, out domains[i]))
                return false;

            if (domains[i].Length == 0 || combinationCount > 256 / domains[i].Length)
                return false;

            combinationCount *= domains[i].Length;
        }

        var values = new FiniteValue[parameters.Length];
        return AllCombinationsAreCovered(parameterIndex: 0);

        bool AllCombinationsAreCovered(int parameterIndex)
        {
            if (parameterIndex == parameters.Length)
            {
                return patterns.Any(pattern =>
                    pattern.Arguments.Length == parameters.Length &&
                    ArgumentsCoverValues(parameters, pattern.Arguments, values, isTotalPattern));
            }

            foreach (var value in domains[parameterIndex])
            {
                values[parameterIndex] = value;
                if (!AllCombinationsAreCovered(parameterIndex + 1))
                    return false;
            }

            return true;
        }
    }

    private static bool ArgumentsCoverValues(
        ImmutableArray<IParameterSymbol> parameters,
        ImmutableArray<BoundPattern> arguments,
        IReadOnlyList<FiniteValue> values,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        for (var i = 0; i < parameters.Length; i++)
        {
            if (!PatternCoversFiniteValue(parameters[i].Type, arguments[i], values[i], isTotalPattern))
                return false;
        }

        return true;
    }

    private static bool PatternCoversFiniteValue(
        ITypeSymbol type,
        BoundPattern pattern,
        FiniteValue value,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        if (isTotalPattern(type, pattern))
            return true;

        if (pattern is BoundOrPattern orPattern)
        {
            return PatternCoversFiniteValue(type, orPattern.Left, value, isTotalPattern) ||
                   PatternCoversFiniteValue(type, orPattern.Right, value, isTotalPattern);
        }

        if (value is BooleanFiniteValue booleanValue)
            return TryGetBooleanConstant(pattern, out var patternValue) && patternValue == booleanValue.Value;

        if (value is not UnionCaseFiniteValue unionValue || pattern is not BoundCasePattern casePattern)
            return false;

        if (!SameCase(casePattern.CaseSymbol, unionValue.CaseSymbol) ||
            casePattern.Arguments.Length != unionValue.Arguments.Length)
        {
            return false;
        }

        var parameters = unionValue.CaseSymbol.ConstructorParameters;
        for (var i = 0; i < parameters.Length; i++)
        {
            if (!PatternCoversFiniteValue(
                    parameters[i].Type,
                    casePattern.Arguments[i],
                    unionValue.Arguments[i],
                    isTotalPattern))
            {
                return false;
            }
        }

        return true;
    }

    private static bool TryEnumerateFiniteValues(
        ITypeSymbol type,
        int depth,
        out ImmutableArray<FiniteValue> values)
    {
        if (depth > 16)
        {
            values = default;
            return false;
        }

        type = UnwrapAlias(type);
        if (type.SpecialType == SpecialType.System_Boolean)
        {
            values = [new BooleanFiniteValue(false), new BooleanFiniteValue(true)];
            return true;
        }

        var union = type.TryGetUnion() ?? type.TryGetUnionCase()?.Union;
        if (union is null || union.DeclaredCaseTypes.IsDefaultOrEmpty)
        {
            values = default;
            return false;
        }

        var builder = ImmutableArray.CreateBuilder<FiniteValue>();
        foreach (var caseSymbol in union.DeclaredCaseTypes)
        {
            var parameters = caseSymbol.ConstructorParameters;
            if (parameters.Length == 0)
            {
                builder.Add(new UnionCaseFiniteValue(caseSymbol, []));
                continue;
            }

            var domains = new ImmutableArray<FiniteValue>[parameters.Length];
            var caseCombinationCount = 1;
            for (var i = 0; i < parameters.Length; i++)
            {
                if (!TryEnumerateFiniteValues(parameters[i].Type, depth + 1, out domains[i]))
                {
                    values = default;
                    return false;
                }

                if (domains[i].Length == 0 || caseCombinationCount > (256 - builder.Count) / domains[i].Length)
                {
                    values = default;
                    return false;
                }

                caseCombinationCount *= domains[i].Length;
            }

            var arguments = new FiniteValue[parameters.Length];
            AddCombinations(parameterIndex: 0);

            void AddCombinations(int parameterIndex)
            {
                if (parameterIndex == parameters.Length)
                {
                    builder.Add(new UnionCaseFiniteValue(caseSymbol, [.. arguments]));
                    return;
                }

                foreach (var argument in domains[parameterIndex])
                {
                    arguments[parameterIndex] = argument;
                    AddCombinations(parameterIndex + 1);
                }
            }
        }

        values = builder.ToImmutable();
        return true;
    }

    private static bool PatternsCoverType(
        ITypeSymbol type,
        ImmutableArray<BoundPattern> patterns,
        Func<ITypeSymbol, BoundPattern, bool> isTotalPattern)
    {
        if (patterns.Any(pattern => isTotalPattern(type, pattern)))
            return true;

        if (UnwrapAlias(type).SpecialType == SpecialType.System_Boolean)
        {
            var coversTrue = false;
            var coversFalse = false;

            foreach (var pattern in patterns)
            {
                if (!TryGetBooleanConstant(pattern, out var value))
                    continue;

                coversTrue |= value;
                coversFalse |= !value;
            }

            return coversTrue && coversFalse;
        }

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

    private static bool TryGetBooleanConstant(BoundPattern pattern, out bool value)
    {
        if (pattern is BoundConstantPattern constant)
        {
            if (TryGetBooleanConstant(constant.Expression, out value))
                return true;

            if (constant.ConstantValue is bool constantValue)
            {
                value = constantValue;
                return true;
            }
        }

        value = default;
        return false;
    }

    private static bool TryGetBooleanConstant(BoundExpression? expression, out bool value)
    {
        switch (expression)
        {
            case BoundLiteralExpression { Value: bool literalValue }:
                value = literalValue;
                return true;
            case BoundConversionExpression conversion:
                return TryGetBooleanConstant(conversion.Expression, out value);
            default:
                value = default;
                return false;
        }
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

    private abstract record FiniteValue;

    private sealed record BooleanFiniteValue(bool Value) : FiniteValue;

    private sealed record UnionCaseFiniteValue(
        IUnionCaseTypeSymbol CaseSymbol,
        ImmutableArray<FiniteValue> Arguments) : FiniteValue;
}
