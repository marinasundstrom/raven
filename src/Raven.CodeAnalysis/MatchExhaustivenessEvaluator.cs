using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class MatchExhaustivenessEvaluator
{
    private readonly Compilation _compilation;
    private readonly Func<SyntaxNode, BoundNode?> _getBoundNode;

    public MatchExhaustivenessEvaluator(Compilation compilation, Func<SyntaxNode, BoundNode?> getBoundNode)
    {
        _compilation = compilation;
        _getBoundNode = getBoundNode;
    }

    public MatchExhaustivenessInfo Evaluate(
        SyntaxNode matchSyntax,
        BoundMatchExpression matchExpression,
        MatchExhaustivenessOptions options)
    {
        return Evaluate(matchSyntax, matchExpression.Expression, matchExpression.Arms, options);
    }

    public MatchExhaustivenessInfo Evaluate(
        SyntaxNode matchSyntax,
        BoundMatchStatement matchStatement,
        MatchExhaustivenessOptions options)
    {
        return Evaluate(matchSyntax, matchStatement.Expression, matchStatement.Arms, options);
    }

    internal MatchExhaustivenessInfo Evaluate(
        SyntaxNode matchSyntax,
        BoundExpression scrutinee,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options)
    {
        if (scrutinee.Type is not TypeSymbol scrutineeType)
            return new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false);

        scrutineeType = UnwrapAlias(scrutineeType);

        if (scrutineeType.TypeKind == TypeKind.Error)
            return new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false);

        var hasCatchAll = arms.Any(arm =>
            BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard) &&
            IsCatchAllPattern(scrutineeType, arm.Pattern));

        ImmutableArray<string> missingCases;

        var nullableUnderlyingType = scrutineeType.GetNonNullableType();
        var nullableUnderlyingUnion = !SymbolEqualityComparer.Default.Equals(nullableUnderlyingType, scrutineeType)
            ? nullableUnderlyingType.TryGetUnion() ?? nullableUnderlyingType.TryGetUnionCase()?.Union
            : null;

        if (nullableUnderlyingUnion is not null)
        {
            missingCases = GetMissingNullableUnionCases(matchSyntax, scrutinee, scrutineeType, nullableUnderlyingType, arms, nullableUnderlyingUnion, options);
        }
        else if (!SymbolEqualityComparer.Default.Equals(nullableUnderlyingType, scrutineeType))
        {
            missingCases = GetMissingNullableCases(
                matchSyntax,
                scrutinee,
                scrutineeType,
                nullableUnderlyingType,
                arms,
                options);
        }
        else
        {
            missingCases = GetMissingNonNullableCases(matchSyntax, scrutinee, scrutineeType, arms, options);
        }

        return new MatchExhaustivenessInfo(missingCases.IsDefaultOrEmpty, missingCases, hasCatchAll);
    }

    private ImmutableArray<string> GetMissingNonNullableCases(
        SyntaxNode matchSyntax,
        BoundExpression scrutinee,
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options)
    {
        if (IsBooleanType(scrutineeType))
            return GetMissingBooleanCases(scrutineeType, arms, options);

        if (scrutineeType.SpecialType == SpecialType.System_Unit)
        {
            return GetMissingSingletonCase(
                scrutineeType,
                arms,
                options,
                "()",
                pattern => IsTotalPattern(scrutineeType, pattern));
        }

        if (scrutineeType.TypeKind == TypeKind.Null)
        {
            return GetMissingSingletonCase(
                scrutineeType,
                arms,
                options,
                "null",
                pattern => PatternCoversNull(scrutineeType, pattern));
        }

        if (scrutineeType is StructuralUnionTypeSymbol structuralUnion)
            return GetMissingUnionMemberCases(scrutineeType, arms, structuralUnion.ConstituentTypes, options);

        var discriminatedUnion = scrutineeType.TryGetUnion()
            ?? scrutineeType.TryGetUnionCase()?.Union;

        if (discriminatedUnion is not null)
            return GetMissingDiscriminatedUnionCases(matchSyntax, scrutinee, scrutineeType, arms, discriminatedUnion, options);

        if (scrutineeType is NamedTypeSymbol { TypeKind: TypeKind.Enum } enumType)
            return GetMissingEnumCases(scrutineeType, arms, enumType, options);

        if (TypeCoverageHelper.TryGetSealedHierarchy(scrutineeType, out var sealedRoot))
            return GetMissingSealedHierarchyCases(scrutineeType, arms, sealedRoot, options);

        if (TryGetMissingFiniteStructuralCases(scrutineeType, arms, options, out var finiteMissingCases))
            return finiteMissingCases;

        if (NumericInterval.TryGetDomain(scrutineeType.SpecialType, out var domain))
            return GetMissingNumericRangeCases(scrutineeType, arms, domain, options);

        var hasCatchAll = arms.Any(arm =>
            BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard) &&
            IsCatchAllPattern(scrutineeType, arm.Pattern));
        var hasCatchAllIgnoringOption = hasCatchAll && !options.IgnoreCatchAllPatterns;
        return hasCatchAllIgnoringOption ? ImmutableArray<string>.Empty : ImmutableArray.Create("_");
    }

    private static ImmutableArray<string> GetMissingSingletonCase(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options,
        string display,
        Func<BoundPattern, bool> coversValue)
    {
        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;
            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;
            if (coversValue(arm.Pattern))
                return ImmutableArray<string>.Empty;
        }

        return ImmutableArray.Create(display);
    }

    internal static bool IsDiscardPattern(BoundPattern pattern)
    {
        return pattern is BoundDiscardPattern ||
            pattern is BoundDeclarationPattern { Designator: BoundDiscardDesignator };
    }

    internal static bool IsCatchAllPattern(TypeSymbol scrutineeType, BoundPattern pattern)
    {
        if (pattern is BoundConstantPattern || pattern is BoundComparisonPattern || pattern is BoundRangePattern)
            return false;

        switch (pattern)
        {
            case BoundGuardedPattern guardedPattern:
                return BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression) &&
                       IsCatchAllPattern(scrutineeType, guardedPattern.Pattern) &&
                       (guardedPattern.GuardPattern is null ||
                        IsCatchAllPattern(scrutineeType, guardedPattern.GuardPattern));
            case BoundDiscardPattern:
                return true;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (ArePatternTypesEquivalent(declaredType, scrutineeType))
                        return true;

                    return declaredType.SpecialType == SpecialType.System_Object;
                }
            case BoundOrPattern orPattern:
                return IsCatchAllPattern(scrutineeType, orPattern.Left) ||
                       IsCatchAllPattern(scrutineeType, orPattern.Right);
            case BoundPositionalPattern tuplePattern:
                {
                    if (IsTotalRestOnlySequencePattern(scrutineeType, tuplePattern))
                        return true;

                    var elementTypes = GetTupleElementTypes(scrutineeType);

                    if (elementTypes.Length == 0 && tuplePattern.Elements.Length == 0)
                        return true;

                    if (elementTypes.Length != tuplePattern.Elements.Length)
                        return false;

                    for (var i = 0; i < tuplePattern.Elements.Length; i++)
                    {
                        if (!IsCatchAllPattern(elementTypes[i], tuplePattern.Elements[i]))
                            return false;
                    }

                    return true;
                }
        }

        return false;
    }

    private ImmutableArray<string> GetMissingBooleanCases(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options)
    {
        var remaining = BooleanCoverage.All;

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            var covered = GetBooleanCoverage(arm.Pattern);
            remaining &= ~covered;

            if (remaining == BooleanCoverage.None)
                break;
        }

        if (remaining == BooleanCoverage.None)
            return ImmutableArray<string>.Empty;

        var builder = ImmutableArray.CreateBuilder<string>();

        if ((remaining & BooleanCoverage.True) != 0)
            builder.Add("true");

        if ((remaining & BooleanCoverage.False) != 0)
            builder.Add("false");

        return builder.ToImmutable();
    }

    private ImmutableArray<string> GetMissingNullableCases(
        SyntaxNode matchSyntax,
        BoundExpression scrutinee,
        TypeSymbol nullableType,
        TypeSymbol underlyingType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options)
    {
        var nullRemaining = true;
        var valueRemaining = true;

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(nullableType, arm.Pattern))
                continue;

            nullRemaining &= !PatternCoversNull(nullableType, arm.Pattern);
            valueRemaining &= !IsTotalPattern(underlyingType, arm.Pattern, assumeNonNull: true);
        }

        var builder = ImmutableArray.CreateBuilder<string>();
        if (valueRemaining)
        {
            builder.AddRange(GetMissingNonNullableCases(
                matchSyntax,
                scrutinee,
                underlyingType,
                arms,
                options));
        }

        if (nullRemaining)
            builder.Add("null");

        return builder.ToImmutable();
    }

    private bool TryGetMissingFiniteStructuralCases(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options,
        out ImmutableArray<string> missingCases)
    {
        var patterns = arms
            .Where(arm => BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
            .Where(arm => !options.IgnoreCatchAllPatterns || !IsCatchAllPattern(scrutineeType, arm.Pattern))
            .Select(arm => arm.Pattern);

        if (!NestedUnionPatternCoverage.TryAreFiniteValuesCovered(
                scrutineeType,
                patterns,
                (type, pattern) => IsTotalPattern(type, pattern),
                out var areCovered))
        {
            missingCases = default;
            return false;
        }

        missingCases = areCovered ? ImmutableArray<string>.Empty : ImmutableArray.Create("_");
        return true;
    }

    private ImmutableArray<string> GetMissingDiscriminatedUnionCases(
        SyntaxNode matchSyntax,
        BoundExpression scrutinee,
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        UnionSymbol union,
        MatchExhaustivenessOptions options)
    {
        if (union.DeclaredCaseTypes.IsDefaultOrEmpty && !union.MemberTypes.IsDefaultOrEmpty)
            return GetMissingParenthesizedUnionCases(matchSyntax, scrutinee, scrutineeType, arms, union, options);

        var finitePatterns = arms
            .Where(arm => BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
            .Where(arm => !options.IgnoreCatchAllPatterns || !IsCatchAllPattern(scrutineeType, arm.Pattern))
            .Select(arm => arm.Pattern);
        if (NestedUnionPatternCoverage.TryAreFiniteValuesCovered(
                scrutineeType,
                finitePatterns,
                (type, pattern) => IsTotalPattern(type, pattern),
                out var finiteValuesCovered) &&
            finiteValuesCovered)
        {
            return ImmutableArray<string>.Empty;
        }

        var remaining = new HashSet<UnionCaseTypeSymbol>(union.DeclaredCaseTypes, SymbolReferenceComparer<UnionCaseTypeSymbol>.Instance);
        var casePatterns = new Dictionary<UnionCaseTypeSymbol, List<BoundCasePattern>>(SymbolReferenceComparer<UnionCaseTypeSymbol>.Instance);
        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            RemoveCoveredCases(remaining, arm.Pattern, union);
            NestedUnionPatternCoverage.AccumulateAndRemoveCoveredCase(
                remaining,
                arm.Pattern,
                union,
                casePatterns,
                (type, pattern) => IsTotalPattern(type, pattern));
            if (remaining.Count == 0)
                break;
        }

        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var caseSymbol in remaining)
        {
            var boundCasePatterns = arms
                .Where(arm => BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                .Select(arm => arm.Pattern)
                .OfType<BoundCasePattern>()
                .Where(pattern => string.Equals(pattern.CaseSymbol.Name, caseSymbol.Name, StringComparison.Ordinal))
                .ToArray();
            var payloadPatterns = arms
                .Where(arm => BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                .SelectMany(arm => EnumerateCasePayloadPatterns(arm.Pattern, union, caseSymbol.Name))
                .ToArray();
            if (caseSymbol.ConstructorParameters.Length == 1 &&
                (boundCasePatterns.Length == 0 || boundCasePatterns.All(pattern => pattern.Arguments.Length == 1)) &&
                TryGetMissingCasePayloads(
                    scrutineeType,
                    boundCasePatterns.FirstOrDefault()?.CaseSymbol.ConstructorParameters.FirstOrDefault()?.Type ??
                        caseSymbol.ConstructorParameters[0].Type,
                    payloadPatterns,
                    out var missingPayloads))
            {
                builder.AddRange(missingPayloads.Select(payload =>
                    MatchCaseDisplay.ForDiscriminatedUnionCasePayload(caseSymbol, payload)));
            }
            else
            {
                builder.Add(MatchCaseDisplay.ForDiscriminatedUnionCase(caseSymbol));
            }
        }

        return builder
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    private IEnumerable<BoundPattern> EnumerateCasePayloadPatterns(
        BoundPattern pattern,
        UnionSymbol union,
        string caseName)
    {
        switch (pattern)
        {
            case BoundCasePattern casePattern
                when string.Equals(casePattern.CaseSymbol.Name, caseName, StringComparison.Ordinal):
                foreach (var argument in casePattern.Arguments)
                    yield return argument;
                break;
            case BoundUnionMemberPattern unionMemberPattern:
                var matchedCase = TryResolveCoveredUnionCase(union, unionMemberPattern.MemberType, unionMemberPattern.TryGetMethod);
                if (matchedCase is not null &&
                    string.Equals(matchedCase.Name, caseName, StringComparison.Ordinal) &&
                    unionMemberPattern.Pattern is BoundDeconstructPattern memberDeconstructPattern)
                {
                    foreach (var argument in memberDeconstructPattern.Arguments)
                        yield return argument;
                }
                break;
            case BoundDeconstructPattern deconstructPattern:
                var deconstructedType = UnwrapAlias(deconstructPattern.NarrowedType ?? deconstructPattern.ReceiverType);
                if (string.Equals(deconstructedType.Name, caseName, StringComparison.Ordinal))
                {
                    foreach (var argument in deconstructPattern.Arguments)
                        yield return argument;
                }
                break;
            case BoundOrPattern orPattern:
                foreach (var nested in EnumerateCasePayloadPatterns(orPattern.Left, union, caseName))
                    yield return nested;
                foreach (var nested in EnumerateCasePayloadPatterns(orPattern.Right, union, caseName))
                    yield return nested;
                break;
        }
    }

    private bool TryGetMissingCasePayloads(
        TypeSymbol scrutineeType,
        TypeSymbol? declaredPayloadType,
        IReadOnlyList<BoundPattern> patterns,
        out ImmutableArray<TypeSymbol> missingPayloads)
    {
        var payloadType = declaredPayloadType ?? patterns
            .OfType<BoundUnionMemberPattern>()
            .Select(pattern => pattern.UnionType)
            .FirstOrDefault() ?? patterns.FirstOrDefault()?.Type;
        if (payloadType is TypeParameterSymbol typeParameter &&
            scrutineeType is NamedTypeSymbol namedScrutinee &&
            typeParameter.Ordinal < namedScrutinee.TypeArguments.Length)
        {
            payloadType = namedScrutinee.TypeArguments[typeParameter.Ordinal];
        }

        if (payloadType is null)
        {
            missingPayloads = default;
            return false;
        }

        payloadType = UnwrapAlias(payloadType);
        var allPayloads = payloadType switch
        {
            _ when payloadType.TryGetUnion() is { } payloadUnion =>
                UnionContentNullability.GetPatternDomainTypes(payloadUnion, _compilation.NullTypeSymbol),
            _ => ImmutableArray<TypeSymbol>.Empty,
        };
        if (allPayloads.IsDefaultOrEmpty)
        {
            missingPayloads = default;
            return false;
        }
        var remainingPayloads = allPayloads
            .Where(payload => !patterns.Any(pattern =>
                PatternCoversPayloadType(payload, pattern)))
            .ToImmutableArray();

        // An empty result is successful analysis: every payload is covered.
        // Returning false here would make the caller report the entire case as
        // missing, conflating complete coverage with an unsupported domain.
        missingPayloads = remainingPayloads;
        return true;
    }

    private bool PatternCoversPayloadType(TypeSymbol payloadType, BoundPattern pattern)
    {
        if (NestedUnionPatternCoverage.TryGetParameterlessUnionCaseValue(pattern, out var constantCase) &&
            payloadType.TryGetUnionCase() is { } constantPayloadCase &&
            NestedUnionPatternCoverage.AreSameCase(constantPayloadCase, constantCase))
        {
            return true;
        }

        if (pattern is BoundCasePattern casePattern &&
            payloadType.TryGetUnionCase() is { } payloadCase &&
            payloadCase.Ordinal == casePattern.CaseSymbol.Ordinal &&
            AreSameUnionPatternTarget(UnwrapAlias(payloadCase.Union), UnwrapAlias(casePattern.CaseSymbol.Union)))
        {
            return CasePatternCoversAllArguments(payloadCase, casePattern);
        }

        return GetTypePatternCoverage(payloadType, pattern) == TypePatternCoverage.All;
    }

    private ImmutableArray<string> GetMissingNullableUnionCases(
        SyntaxNode matchSyntax,
        BoundExpression scrutinee,
        TypeSymbol nullableScrutineeType,
        TypeSymbol underlyingScrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        UnionSymbol union,
        MatchExhaustivenessOptions options)
    {
        if (union.DeclaredCaseTypes.IsDefaultOrEmpty && !union.MemberTypes.IsDefaultOrEmpty)
            return GetMissingNullableParenthesizedUnionCases(nullableScrutineeType, arms, union, options);

        var builder = ImmutableArray.CreateBuilder<string>();

        builder.AddRange(GetMissingDiscriminatedUnionCases(
            matchSyntax,
            scrutinee,
            underlyingScrutineeType,
            arms,
            union,
            options));

        if (!NullableNullCovered(nullableScrutineeType, arms, options))
            builder.Add("null");

        return builder.ToImmutable();
    }

    private ImmutableArray<string> GetMissingNullableParenthesizedUnionCases(
        TypeSymbol nullableScrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        UnionSymbol union,
        MatchExhaustivenessOptions options)
    {
        var memberTypes = UnionContentNullability.GetPatternDomainTypes(union, _compilation.NullTypeSymbol);
        if (!memberTypes.Any(static type => UnwrapAlias(type).TypeKind == TypeKind.Null))
            memberTypes = memberTypes.Add(_compilation.NullTypeSymbol);

        var remaining = new HashSet<TypeSymbol>(memberTypes, TypeSymbolReferenceComparer.Instance);
        var literalCoverage = CreateLiteralCoverage(remaining);

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(nullableScrutineeType, arm.Pattern))
                continue;

            RemoveCoveredUnionMembers(remaining, arm.Pattern, literalCoverage);

            if (remaining.Count == 0)
                break;
        }

        if (literalCoverage is not null && literalCoverage.Count > 0)
        {
            foreach (var (type, constants) in literalCoverage)
            {
                if (remaining.Contains(type))
                {
                    var literalMissing = GetMissingLiteralCoverage(type, constants);
                    if (!literalMissing.IsDefaultOrEmpty)
                        return literalMissing;
                }
            }
        }

        return remaining
            .Select(member => member.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    private ImmutableArray<string> GetMissingParenthesizedUnionCases(
        SyntaxNode matchSyntax,
        BoundExpression scrutinee,
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        UnionSymbol union,
        MatchExhaustivenessOptions options)
    {
        var memberTypes = UnionContentNullability.GetPatternDomainTypes(union, _compilation.NullTypeSymbol);
        return GetMissingUnionMemberCases(scrutineeType, arms, memberTypes, options);
    }

    private ImmutableArray<string> GetMissingUnionMemberCases(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        ImmutableArray<TypeSymbol> memberTypes,
        MatchExhaustivenessOptions options)
    {
        var remaining = new HashSet<TypeSymbol>(memberTypes, TypeSymbolReferenceComparer.Instance);
        var literalCoverage = CreateLiteralCoverage(remaining);

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            RemoveCoveredUnionMembers(remaining, arm.Pattern, literalCoverage);
            if (remaining.Count == 0)
                break;
        }

        var builder = ImmutableArray.CreateBuilder<string>();

        if (literalCoverage is not null && literalCoverage.Count > 0)
        {
            foreach (var (type, constants) in literalCoverage)
            {
                if (remaining.Contains(type))
                {
                    var literalMissing = GetMissingLiteralCoverage(type, constants);
                    if (!literalMissing.IsDefaultOrEmpty)
                    {
                        builder.AddRange(literalMissing);
                        return builder.ToImmutable();
                    }
                }
            }
        }

        builder.AddRange(remaining
            .Select(member => member.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .OrderBy(name => name, StringComparer.Ordinal));

        return builder.ToImmutable();
    }

    private ImmutableArray<string> GetMissingEnumCases(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        NamedTypeSymbol enumType,
        MatchExhaustivenessOptions options)
    {
        var remaining = new HashSet<FieldSymbol>(GetEnumMembers(enumType), SymbolReferenceComparer<FieldSymbol>.Instance);

        if (remaining.Count == 0)
            return ImmutableArray<string>.Empty;

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            RemoveCoveredEnumMembers(remaining, enumType, arm.Pattern);

            if (remaining.Count == 0)
                break;
        }

        return remaining
            .Select(field => field.Name)
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    private ImmutableArray<string> GetMissingSealedHierarchyCases(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        NamedTypeSymbol sealedRoot,
        MatchExhaustivenessOptions options)
    {
        var projectedHierarchy = TypeCoverageHelper.TryGetSealedHierarchy(
            scrutineeType,
            out _,
            out var constrainedHierarchy)
            ? constrainedHierarchy
            : sealedRoot;
        var leafTypes = TypeCoverageHelper.GetSealedHierarchyCoverageTypes(sealedRoot, projectedHierarchy);

        if (leafTypes.IsEmpty)
            return ImmutableArray<string>.Empty;

        var remaining = new HashSet<TypeSymbol>(leafTypes.Cast<TypeSymbol>(), TypeSymbolReferenceComparer.Instance);

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            RemoveCoveredUnionMembers(remaining, arm.Pattern);

            if (remaining.Count == 0)
                break;
        }

        return remaining
            .Select(member => member.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    private ImmutableArray<string> GetMissingLiteralCoverage(TypeSymbol type, HashSet<object?> constants)
    {
        var targetType = UnwrapAlias(type);

        if (targetType.SpecialType != SpecialType.System_Boolean)
            return ImmutableArray<string>.Empty;

        var builder = ImmutableArray.CreateBuilder<string>();

        if (!constants.Contains(true))
            builder.Add("true");

        if (!constants.Contains(false))
            builder.Add("false");

        return builder.ToImmutable();
    }

    private BooleanCoverage GetBooleanCoverage(BoundPattern pattern)
    {
        switch (pattern)
        {
            case BoundGuardedPattern guardedPattern:
                if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression))
                    return BooleanCoverage.None;

                var guardedCoverage = GetBooleanCoverage(guardedPattern.Pattern);
                return guardedPattern.GuardPattern is null
                    ? guardedCoverage
                    : guardedCoverage & GetBooleanCoverage(guardedPattern.GuardPattern);
            case BoundDiscardPattern:
                return BooleanCoverage.All;
            case BoundDeclarationPattern declaration when IsBooleanType(declaration.DeclaredType):
                return BooleanCoverage.All;
            case BoundConstantPattern constant when TryGetLiteralBoolConstant(constant, out var value):
                return value ? BooleanCoverage.True : BooleanCoverage.False;
            case BoundOrPattern orPattern:
                return GetBooleanCoverage(orPattern.Left) | GetBooleanCoverage(orPattern.Right);
            case BoundAndPattern andPattern:
                return GetBooleanCoverage(andPattern.Left) & GetBooleanCoverage(andPattern.Right);
            case BoundNotPattern notPattern:
                return BooleanCoverage.All & ~GetBooleanCoverage(notPattern.Pattern);
            default:
                return BooleanCoverage.None;
        }
    }

    private bool IsTotalPattern(TypeSymbol inputType, BoundPattern pattern, bool assumeNonNull = false)
    {
        inputType = UnwrapAlias(inputType);

        switch (pattern)
        {
            case BoundGuardedPattern guardedPattern:
                return BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression) &&
                       IsTotalPattern(inputType, guardedPattern.Pattern, assumeNonNull) &&
                       (guardedPattern.GuardPattern is null ||
                        IsTotalPattern(inputType, guardedPattern.GuardPattern, assumeNonNull));
            case BoundDiscardPattern:
                return true;
            case BoundConstantPattern constant:
                return inputType.SpecialType == SpecialType.System_Unit &&
                       constant.Expression is BoundUnitExpression;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (declaredType.TypeKind == TypeKind.Error || inputType.TypeKind == TypeKind.Error)
                        return true;

                    if (ArePatternTypesEquivalent(declaredType, inputType))
                        return true;

                    return IsAssignable(declaredType, inputType);
                }
            case BoundPositionalPattern tuplePattern:
                {
                    if (IsTotalRestOnlySequencePattern(inputType, tuplePattern))
                        return true;

                    var elementTypes = GetTupleElementTypes(inputType);

                    if (elementTypes.Length == 0)
                    {
                        return inputType.SpecialType == SpecialType.System_Unit &&
                               tuplePattern.Elements.Length == 0;
                    }

                    if (elementTypes.Length != tuplePattern.Elements.Length)
                        return false;

                    for (var i = 0; i < tuplePattern.Elements.Length; i++)
                    {
                        if (!IsTotalPattern(elementTypes[i], tuplePattern.Elements[i], assumeNonNull))
                            return false;
                    }

                    return true;
                }
            case BoundPropertyPattern propertyPattern:
                {
                    if (!assumeNonNull && CanBeNull(inputType))
                        return false;

                    if (propertyPattern.NarrowedType is not null &&
                        !IsAssignable(propertyPattern.NarrowedType, inputType) &&
                        !(assumeNonNull && ArePatternTypesEquivalent(propertyPattern.NarrowedType, inputType)))
                    {
                        return false;
                    }

                    foreach (var property in propertyPattern.Properties)
                    {
                        if (!IsTotalPattern(property.Type, property.Pattern, assumeNonNull))
                            return false;
                    }

                    return true;
                }
            case BoundDeconstructPattern deconstructPattern:
                {
                    if (!assumeNonNull && CanBeNull(inputType))
                        return false;

                    if (deconstructPattern.NarrowedType is not null &&
                        !IsAssignable(deconstructPattern.NarrowedType, inputType) &&
                        !(assumeNonNull && ArePatternTypesEquivalent(deconstructPattern.NarrowedType, inputType)))
                    {
                        return false;
                    }

                    var parameters = deconstructPattern.DeconstructMethod.Parameters;
                    var parameterOffset = deconstructPattern.DeconstructMethod.IsExtensionMethod ? 1 : 0;
                    var parameterCount = parameters.Length - parameterOffset;
                    for (var i = 0; i < parameterCount; i++)
                    {
                        if (!IsTotalPattern(parameters[i + parameterOffset].Type, deconstructPattern.Arguments[i], assumeNonNull))
                            return false;
                    }

                    return true;
                }
            case BoundOrPattern orPattern:
                return IsTotalPattern(inputType, orPattern.Left, assumeNonNull) ||
                       IsTotalPattern(inputType, orPattern.Right, assumeNonNull);
            case BoundAndPattern andPattern:
                return IsTotalPattern(inputType, andPattern.Left, assumeNonNull) &&
                       IsTotalPattern(inputType, andPattern.Right, assumeNonNull);
            case BoundNotPattern notPattern when assumeNonNull && IsNullPattern(notPattern.Pattern):
                return true;
            default:
                return false;
        }
    }

    private static bool TryGetLiteralBoolConstant(BoundPattern pattern, out bool value)
    {
        value = default;

        if (pattern is not BoundConstantPattern constant)
            return false;

        if (TryGetExpressionBoolConstant(constant.Expression, out value))
            return true;

        if (constant.ConstantValue is not bool b)
            return false;

        value = b;
        return true;
    }

    private static bool TryGetExpressionBoolConstant(BoundExpression? expression, out bool value)
    {
        switch (expression)
        {
            case BoundLiteralExpression { Value: bool b }:
                value = b;
                return true;
            case BoundConversionExpression conversion:
                return TryGetExpressionBoolConstant(conversion.Expression, out value);
            default:
                value = default;
                return false;
        }
    }

    private void RemoveCoveredUnionMembers(
        HashSet<TypeSymbol> remaining,
        BoundPattern pattern,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage = null)
    {
        UpdateBooleanLiteralCoverage(remaining, pattern, literalCoverage);

        switch (pattern)
        {
            case BoundGuardedPattern:
                RemoveMembersWithTotalTypeCoverage(remaining, pattern, literalCoverage);
                break;
            case BoundDiscardPattern:
                remaining.Clear();
                literalCoverage?.Clear();
                break;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);
                    if (declaredType.TypeKind == TypeKind.Null)
                    {
                        RemoveNullMember(remaining, literalCoverage);
                        break;
                    }

                    RemoveMembersAssignableToPattern(remaining, declaredType, literalCoverage);
                    break;
                }
            case BoundConstantPattern constant:
                {
                    if (constant.Expression is not null)
                    {
                        if (IsNullLiteralPatternExpression(constant.Expression))
                            RemoveNullMember(remaining, literalCoverage);

                        break;
                    }

                    if (constant.LiteralType is null)
                        break;

                    var literalType = (LiteralTypeSymbol)UnwrapAlias(constant.LiteralType);

                    if (TryUpdateLiteralCoverage(remaining, literalCoverage, literalType))
                        break;

                    if (constant.ConstantValue is null)
                    {
                        RemoveNullMember(remaining, literalCoverage);

                        break;
                    }

                    foreach (var candidate in remaining.ToArray())
                    {
                        var candidateType = UnwrapAlias(candidate);

                        if (ArePatternTypesEquivalent(candidateType, literalType))
                        {
                            remaining.Remove(candidate);
                            literalCoverage?.Remove(candidate);
                        }
                    }

                    break;
                }
            case BoundUnionMemberPattern unionMemberPattern:
                RemoveUnionMemberPatternCoverage(remaining, unionMemberPattern, literalCoverage);
                break;
            case BoundOrPattern orPattern:
                RemoveCoveredUnionMembers(remaining, orPattern.Left, literalCoverage);
                RemoveCoveredUnionMembers(remaining, orPattern.Right, literalCoverage);
                break;
            case BoundAndPattern:
            case BoundNotPattern:
                RemoveMembersWithTotalTypeCoverage(remaining, pattern, literalCoverage);
                break;
            case BoundPositionalPattern tuplePattern:
                RemoveMembersCoveredByTotalPattern(remaining, tuplePattern, literalCoverage);
                break;
            case BoundDeconstructPattern deconstructPattern:
                RemoveMembersCoveredByTotalPattern(remaining, deconstructPattern, literalCoverage);
                break;
            case BoundPropertyPattern propertyPattern:
                RemoveMembersCoveredByTotalPattern(remaining, propertyPattern, literalCoverage);
                break;
        }
    }

    private void UpdateBooleanLiteralCoverage(
        HashSet<TypeSymbol> remaining,
        BoundPattern pattern,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage)
    {
        if (literalCoverage is null)
            return;

        var coverage = GetBooleanCoverage(pattern);
        if (coverage == BooleanCoverage.None)
            return;

        foreach (var candidate in remaining.ToArray())
        {
            if (!IsBooleanType(candidate) || !literalCoverage.TryGetValue(candidate, out var constants))
                continue;

            if ((coverage & BooleanCoverage.True) != 0)
                constants.Add(true);
            if ((coverage & BooleanCoverage.False) != 0)
                constants.Add(false);

            if (constants.Contains(true) && constants.Contains(false))
            {
                remaining.Remove(candidate);
                literalCoverage.Remove(candidate);
            }
        }
    }

    private void RemoveMembersWithTotalTypeCoverage(
        HashSet<TypeSymbol> remaining,
        BoundPattern pattern,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage)
    {
        foreach (var candidate in remaining.ToArray())
        {
            if (GetTypePatternCoverage(UnwrapAlias(candidate), pattern) != TypePatternCoverage.All)
                continue;

            remaining.Remove(candidate);
            literalCoverage?.Remove(candidate);
        }
    }

    private TypePatternCoverage GetTypePatternCoverage(TypeSymbol candidateType, BoundPattern pattern)
    {
        candidateType = UnwrapAlias(candidateType);

        switch (pattern)
        {
            case BoundGuardedPattern guardedPattern:
                {
                    if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression))
                        return TypePatternCoverage.Some;

                    var coverage = GetTypePatternCoverage(candidateType, guardedPattern.Pattern);
                    return guardedPattern.GuardPattern is null
                        ? coverage
                        : AndCoverage(coverage, GetTypePatternCoverage(candidateType, guardedPattern.GuardPattern));
                }
            case BoundDiscardPattern:
                return TypePatternCoverage.All;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);
                    if (IsTotalPattern(candidateType, declaration))
                        return TypePatternCoverage.All;
                    if (candidateType.TypeKind == TypeKind.Null || declaredType.TypeKind == TypeKind.Null)
                        return TypePatternCoverage.None;
                    if (IsAssignable(candidateType, declaredType))
                        return TypePatternCoverage.Some;
                    if (candidateType.IsValueType || candidateType is NamedTypeSymbol { IsClosed: true })
                        return TypePatternCoverage.None;
                    return TypePatternCoverage.Some;
                }
            case BoundConstantPattern constant:
                {
                    if (IsNullPattern(constant))
                        return candidateType.TypeKind == TypeKind.Null ? TypePatternCoverage.All : TypePatternCoverage.None;
                    if (IsBooleanType(candidateType))
                    {
                        return GetBooleanCoverage(constant) switch
                        {
                            BooleanCoverage.All => TypePatternCoverage.All,
                            BooleanCoverage.None => TypePatternCoverage.None,
                            _ => TypePatternCoverage.Some
                        };
                    }

                    if (constant.LiteralType is { } literalType &&
                        !TypeCoverageHelper.LiteralBelongsToType(
                            (LiteralTypeSymbol)UnwrapAlias(literalType),
                            candidateType))
                    {
                        return TypePatternCoverage.None;
                    }

                    if (constant.Expression?.Type is TypeSymbol expressionType)
                    {
                        expressionType = UnwrapAlias(expressionType);
                        if (!IsAssignable(candidateType, expressionType) &&
                            !IsAssignable(expressionType, candidateType) &&
                            (candidateType.IsValueType || candidateType is NamedTypeSymbol { IsClosed: true }))
                        {
                            return TypePatternCoverage.None;
                        }
                    }

                    return TypePatternCoverage.Some;
                }
            case BoundUnionMemberPattern unionMemberPattern:
                {
                    var memberType = UnwrapAlias(unionMemberPattern.MemberType);
                    memberType = UnionContentNullability.GetNonNullContentType(memberType, out _);
                    var nonNullCandidate = UnionContentNullability.GetNonNullContentType(candidateType, out _);
                    if (!ArePatternTypesEquivalent(nonNullCandidate, memberType))
                        return TypePatternCoverage.None;
                    return IsTotalPattern(nonNullCandidate, unionMemberPattern.Pattern, assumeNonNull: true)
                        ? TypePatternCoverage.All
                        : TypePatternCoverage.Some;
                }
            case BoundPositionalPattern:
            case BoundDeconstructPattern:
            case BoundPropertyPattern:
                return IsTotalPattern(candidateType, pattern)
                    ? TypePatternCoverage.All
                    : TypePatternCoverage.Some;
            case BoundOrPattern orPattern:
                return OrCoverage(
                    GetTypePatternCoverage(candidateType, orPattern.Left),
                    GetTypePatternCoverage(candidateType, orPattern.Right));
            case BoundAndPattern andPattern:
                return AndCoverage(
                    GetTypePatternCoverage(candidateType, andPattern.Left),
                    GetTypePatternCoverage(candidateType, andPattern.Right));
            case BoundNotPattern notPattern:
                return GetTypePatternCoverage(candidateType, notPattern.Pattern) switch
                {
                    TypePatternCoverage.None => TypePatternCoverage.All,
                    TypePatternCoverage.All => TypePatternCoverage.None,
                    _ => TypePatternCoverage.Some
                };
            default:
                return TypePatternCoverage.Some;
        }
    }

    private static TypePatternCoverage OrCoverage(TypePatternCoverage left, TypePatternCoverage right)
    {
        if (left == TypePatternCoverage.All || right == TypePatternCoverage.All)
            return TypePatternCoverage.All;
        if (left == TypePatternCoverage.None && right == TypePatternCoverage.None)
            return TypePatternCoverage.None;
        return TypePatternCoverage.Some;
    }

    private static TypePatternCoverage AndCoverage(TypePatternCoverage left, TypePatternCoverage right)
    {
        if (left == TypePatternCoverage.None || right == TypePatternCoverage.None)
            return TypePatternCoverage.None;
        if (left == TypePatternCoverage.All && right == TypePatternCoverage.All)
            return TypePatternCoverage.All;
        return TypePatternCoverage.Some;
    }

    private static bool IsNullLiteralPatternExpression(BoundExpression expression)
        => expression switch
        {
            BoundLiteralExpression { Kind: BoundLiteralExpressionKind.NullLiteral } => true,
            BoundTypeExpression { Type: NullTypeSymbol } => true,
            BoundConversionExpression conversion => IsNullLiteralPatternExpression(conversion.Expression),
            _ => false
        };

    private static void RemoveNullMember(
        HashSet<TypeSymbol> remaining,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage)
    {
        foreach (var candidate in remaining.ToArray())
        {
            var candidateType = UnwrapAlias(candidate);

            if (candidateType.TypeKind == TypeKind.Null)
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
            }
        }
    }

    private static bool NullableNullCovered(
        TypeSymbol nullableScrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options)
    {
        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(nullableScrutineeType, arm.Pattern))
                continue;

            if (PatternCoversNull(nullableScrutineeType, arm.Pattern))
                return true;
        }

        return false;
    }

    private static bool PatternCoversNull(TypeSymbol scrutineeType, BoundPattern pattern)
    {
        if (IsCatchAllPattern(scrutineeType, pattern))
            return true;

        if (IsNullPattern(pattern))
            return true;

        return pattern switch
        {
            BoundGuardedPattern guardedPattern =>
                BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression) &&
                PatternCoversNull(scrutineeType, guardedPattern.Pattern) &&
                (guardedPattern.GuardPattern is null ||
                 PatternCoversNull(scrutineeType, guardedPattern.GuardPattern)),
            BoundOrPattern orPattern =>
                PatternCoversNull(scrutineeType, orPattern.Left) ||
                PatternCoversNull(scrutineeType, orPattern.Right),
            BoundAndPattern andPattern =>
                PatternCoversNull(scrutineeType, andPattern.Left) &&
                PatternCoversNull(scrutineeType, andPattern.Right),
            BoundNotPattern notPattern => !PatternCoversNull(scrutineeType, notPattern.Pattern),
            _ => false
        };
    }

    private static bool IsNullPattern(BoundPattern pattern)
    {
        if (pattern is BoundConstantPattern { Expression: null, ConstantValue: null })
            return true;

        return pattern is BoundConstantPattern { Expression: { } expression } &&
               IsNullLiteralPatternExpression(expression);
    }

    private void RemoveUnionMemberPatternCoverage(
        HashSet<TypeSymbol> remaining,
        BoundUnionMemberPattern unionMemberPattern,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage)
    {
        var patternMemberType = UnwrapAlias(unionMemberPattern.MemberType);
        patternMemberType = UnionContentNullability.GetNonNullContentType(patternMemberType, out _);

        foreach (var candidate in remaining.ToArray())
        {
            var candidateType = UnwrapAlias(candidate);
            candidateType = UnionContentNullability.GetNonNullContentType(candidateType, out _);
            if (!ArePatternTypesEquivalent(candidateType, patternMemberType))
                continue;

            if (!IsTotalPattern(candidateType, unionMemberPattern.Pattern, assumeNonNull: true))
                continue;

            remaining.Remove(candidate);
            literalCoverage?.Remove(candidate);
        }
    }

    private void RemoveCoveredCases(
        HashSet<UnionCaseTypeSymbol> remaining,
        BoundPattern pattern,
        UnionSymbol union)
    {
        switch (pattern)
        {
            case BoundGuardedPattern guardedPattern:
                if (BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression) &&
                    (guardedPattern.GuardPattern is null ||
                     IsTotalPattern((TypeSymbol)union, guardedPattern.GuardPattern)))
                {
                    RemoveCoveredCases(remaining, guardedPattern.Pattern, union);
                }

                break;
            case BoundDiscardPattern:
                remaining.Clear();
                break;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (declaredType.SpecialType == SpecialType.System_Object ||
                        ArePatternTypesEquivalent(declaredType, UnwrapAlias((TypeSymbol)union)))
                    {
                        remaining.Clear();
                        break;
                    }

                    if (declaredType.TryGetUnionCase() is { } declarationCase &&
                        AreSameUnionPatternTarget(UnwrapAlias(declarationCase.Union), UnwrapAlias(union)))
                    {
                        var matchedCase = declarationCase.OriginalDefinition as UnionCaseTypeSymbol ?? declarationCase;
                        remaining.RemoveWhere(candidate =>
                            candidate.Ordinal == matchedCase.Ordinal ||
                            SymbolEqualityComparer.Default.Equals(candidate, matchedCase));
                    }
                    else if (declaredType.TryGetUnion() is { } declarationUnion &&
                        AreSameUnionPatternTarget(UnwrapAlias(declarationUnion), UnwrapAlias(union)))
                    {
                        remaining.Clear();
                    }

                    break;
                }
            case BoundCasePattern casePattern:
                {
                    var matchedCase = union.DeclaredCaseTypes.FirstOrDefault(candidate =>
                        NestedUnionPatternCoverage.AreSameCase(candidate, casePattern.CaseSymbol));
                    if (matchedCase is not null &&
                        CasePatternCoversAllArguments(matchedCase, casePattern))
                    {
                        remaining.RemoveWhere(candidate =>
                            NestedUnionPatternCoverage.AreSameCase(candidate, matchedCase));
                    }

                    break;
                }
            case BoundUnionMemberPattern unionMemberPattern:
                {
                    var matchedCase = TryResolveCoveredUnionCase(union, unionMemberPattern.MemberType, unionMemberPattern.TryGetMethod);
                    if (matchedCase is not null &&
                        IsTotalPattern(unionMemberPattern.MemberType, unionMemberPattern.Pattern, assumeNonNull: true))
                    {
                        remaining.RemoveWhere(candidate =>
                            candidate.Ordinal == matchedCase.Ordinal ||
                            SymbolEqualityComparer.Default.Equals(candidate, matchedCase));
                    }

                    break;
                }
            case BoundOrPattern orPattern:
                RemoveCoveredCases(remaining, orPattern.Left, union);
                RemoveCoveredCases(remaining, orPattern.Right, union);
                break;
            case BoundAndPattern andPattern:
                {
                    var leftUncovered = CreateUnionCaseDomain(union);
                    RemoveCoveredCases(leftUncovered, andPattern.Left, union);

                    var rightUncovered = CreateUnionCaseDomain(union);
                    RemoveCoveredCases(rightUncovered, andPattern.Right, union);

                    leftUncovered.UnionWith(rightUncovered);
                    remaining.RemoveWhere(candidate => !leftUncovered.Contains(candidate));
                    break;
                }
            case BoundNotPattern notPattern:
                {
                    var coveredByNot = CreateUnionCaseDomain(union);
                    RemoveCoveredCases(coveredByNot, notPattern.Pattern, union);
                    remaining.ExceptWith(coveredByNot);
                    break;
                }
        }
    }

    private static HashSet<UnionCaseTypeSymbol> CreateUnionCaseDomain(UnionSymbol union)
        => new(union.DeclaredCaseTypes, SymbolReferenceComparer<UnionCaseTypeSymbol>.Instance);

    private static IEnumerable<FieldSymbol> GetEnumMembers(NamedTypeSymbol enumType)
    {
        var normalizedEnum = (NamedTypeSymbol)UnwrapAlias(enumType);

        return normalizedEnum
            .GetMembers()
            .OfType<FieldSymbol>()
            .Where(field =>
                field.IsConst &&
                ArePatternTypesEquivalent(UnwrapAlias(field.Type), normalizedEnum));
    }

    private void RemoveCoveredEnumMembers(HashSet<FieldSymbol> remaining, NamedTypeSymbol enumType, BoundPattern pattern)
    {
        enumType = (NamedTypeSymbol)UnwrapAlias(enumType);

        if (IsCatchAllPattern(enumType, pattern))
        {
            remaining.Clear();
            return;
        }

        switch (pattern)
        {
            case BoundGuardedPattern guardedPattern:
                if (BoundNodeFacts.MatchArmGuardGuaranteesMatch(guardedPattern.GuardExpression) &&
                    (guardedPattern.GuardPattern is null ||
                     IsTotalPattern(enumType, guardedPattern.GuardPattern)))
                {
                    RemoveCoveredEnumMembers(remaining, enumType, guardedPattern.Pattern);
                }

                break;
            case BoundConstantPattern constant:
                if (TryGetEnumField(constant.Expression, enumType, out var field))
                    remaining.Remove(field);

                break;
            case BoundOrPattern orPattern:
                RemoveCoveredEnumMembers(remaining, enumType, orPattern.Left);
                RemoveCoveredEnumMembers(remaining, enumType, orPattern.Right);
                break;
            case BoundAndPattern andPattern:
                {
                    var leftUncovered = CreateEnumDomain(enumType);
                    RemoveCoveredEnumMembers(leftUncovered, enumType, andPattern.Left);

                    var rightUncovered = CreateEnumDomain(enumType);
                    RemoveCoveredEnumMembers(rightUncovered, enumType, andPattern.Right);

                    leftUncovered.UnionWith(rightUncovered);
                    remaining.RemoveWhere(candidate => !leftUncovered.Contains(candidate));
                    break;
                }
            case BoundNotPattern notPattern:
                {
                    var coveredByNot = CreateEnumDomain(enumType);
                    RemoveCoveredEnumMembers(coveredByNot, enumType, notPattern.Pattern);
                    remaining.ExceptWith(coveredByNot);
                    break;
                }
        }
    }

    private static HashSet<FieldSymbol> CreateEnumDomain(NamedTypeSymbol enumType)
        => new(GetEnumMembers(enumType), SymbolReferenceComparer<FieldSymbol>.Instance);

    private static bool TryGetEnumField(
        BoundExpression? expression,
        NamedTypeSymbol enumType,
        out FieldSymbol field)
    {
        field = null!;

        return expression switch
        {
            BoundFieldAccess fieldAccess => TryBindEnumField(fieldAccess.Field, enumType, out field),
            BoundMemberAccessExpression { Member: FieldSymbol memberField } =>
                TryBindEnumField(memberField, enumType, out field),
            _ => false
        };
    }

    private static bool TryBindEnumField(
        FieldSymbol candidate,
        NamedTypeSymbol enumType,
        out FieldSymbol field)
    {
        field = null!;

        if (!candidate.IsConst ||
            !ArePatternTypesEquivalent(UnwrapAlias(candidate.Type), enumType))
        {
            return false;
        }

        field = candidate;
        return true;
    }

    private bool CasePatternCoversAllArguments(BoundCasePattern casePattern)
        => CasePatternCoversAllArguments(casePattern.CaseSymbol, casePattern);

    private bool CasePatternCoversAllArguments(
        UnionCaseTypeSymbol caseSymbol,
        BoundCasePattern casePattern)
    {
        var parameters = caseSymbol.ConstructorParameters;
        var boundCaseParameters = casePattern.CaseSymbol.ConstructorParameters;
        var argumentCount = Math.Min(parameters.Length, casePattern.Arguments.Length);

        for (var i = 0; i < argumentCount; i++)
        {
            var argument = casePattern.Arguments[i];
            if (IsTotalPattern(parameters[i].Type, argument))
                continue;

            // A case pattern inside its generic union can bind an inferred
            // declaration to the case's projected type parameter, while the
            // exhaustiveness domain carries the union's type parameter. They
            // describe the same complete payload pattern even though those
            // parameter symbols have different owners.
            if (argument is BoundDeclarationPattern &&
                i < boundCaseParameters.Length &&
                IsTotalPattern(boundCaseParameters[i].Type, argument))
            {
                continue;
            }

            return false;
        }

        return true;
    }

    private static UnionCaseTypeSymbol? TryResolveCoveredUnionCase(
        UnionSymbol union,
        TypeSymbol memberType,
        MethodSymbol? tryGetMethod = null)
    {
        if (tryGetMethod is not null && tryGetMethod.Parameters.Length == 1)
        {
            var parameterType = tryGetMethod.Parameters[0].GetByRefElementType();
            var parameterCase = parameterType.TryGetUnionCase();
            if (parameterCase is not null)
            {
                var parameterUnion = UnwrapAlias((TypeSymbol)parameterCase.Union);
                if (AreSameUnionPatternTarget(parameterUnion, UnwrapAlias((TypeSymbol)union)))
                    return parameterCase.OriginalDefinition as UnionCaseTypeSymbol ?? parameterCase;
            }

            if (parameterType is NamedTypeSymbol namedParameterType)
            {
                var nameMatch = union.DeclaredCaseTypes.FirstOrDefault(caseType =>
                    string.Equals(caseType.Name, namedParameterType.Name, StringComparison.Ordinal));
                if (nameMatch is not null)
                    return nameMatch.OriginalDefinition as UnionCaseTypeSymbol ?? nameMatch;
            }
        }

        var normalizedMemberType = UnwrapAlias(memberType);
        var memberCase = normalizedMemberType.TryGetUnionCase();
        if (memberCase is not null)
        {
            var memberUnion = UnwrapAlias((TypeSymbol)memberCase.Union);
            if (AreSameUnionPatternTarget(memberUnion, UnwrapAlias((TypeSymbol)union)))
                return memberCase.OriginalDefinition as UnionCaseTypeSymbol ?? memberCase;
        }

        foreach (var caseType in union.DeclaredCaseTypes)
        {
            var normalizedCaseType = UnwrapAlias((TypeSymbol)((caseType.OriginalDefinition as UnionCaseTypeSymbol) ?? caseType));

            if (ArePatternTypesEquivalent(normalizedCaseType, normalizedMemberType))
                return caseType.OriginalDefinition as UnionCaseTypeSymbol ?? caseType;
        }

        for (var i = 0; i < union.MemberTypes.Length && i < union.DeclaredCaseTypes.Length; i++)
        {
            var normalizedCaseMemberType = UnwrapAlias(union.MemberTypes[i]);
            if (ArePatternTypesEquivalent(normalizedCaseMemberType, normalizedMemberType))
            {
                var caseType = union.DeclaredCaseTypes[i];
                return caseType.OriginalDefinition as UnionCaseTypeSymbol ?? caseType;
            }
        }

        if (normalizedMemberType is NamedTypeSymbol namedMemberType)
        {
            var nameMatch = union.DeclaredCaseTypes.FirstOrDefault(caseType =>
                string.Equals(caseType.Name, namedMemberType.Name, StringComparison.Ordinal));
            if (nameMatch is not null)
                return nameMatch.OriginalDefinition as UnionCaseTypeSymbol ?? nameMatch;
        }

        return null;
    }

    private void RemoveMembersAssignableToPattern(
        HashSet<TypeSymbol> remaining,
        TypeSymbol patternType,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage = null)
    {
        patternType = UnwrapAlias(patternType);
        patternType = UnionContentNullability.GetNonNullContentType(patternType, out _);

        if (patternType.TypeKind == TypeKind.Error)
            return;

        foreach (var candidate in remaining.ToArray())
        {
            var candidateType = UnwrapAlias(candidate);

            if (candidateType.TypeKind == TypeKind.Error)
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
                continue;
            }

            if (candidateType.TypeKind == TypeKind.Null)
                continue;

            if (IsAssignable(patternType, candidateType))
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
                continue;
            }

        }
    }

    private void RemoveMembersCoveredByTotalPattern(
        HashSet<TypeSymbol> remaining,
        BoundPattern pattern,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage)
    {
        foreach (var candidate in remaining.ToArray())
        {
            var candidateType = UnwrapAlias(candidate);
            if (!IsTotalPattern(candidateType, pattern))
                continue;

            remaining.Remove(candidate);
            literalCoverage?.Remove(candidate);
        }
    }

    private static bool IsBooleanType(TypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type.SpecialType == SpecialType.System_Boolean)
            return true;

        if (type is LiteralTypeSymbol literal)
            return IsBooleanType(literal.UnderlyingType);

        return false;
    }

    private static Dictionary<TypeSymbol, HashSet<object?>>? CreateLiteralCoverage(IEnumerable<TypeSymbol> members)
    {
        Dictionary<TypeSymbol, HashSet<object?>>? coverage = null;

        foreach (var member in members)
        {
            var type = UnwrapAlias(member);

            if (TypeCoverageHelper.RequiresLiteralCoverage(type))
            {
                coverage ??= new Dictionary<TypeSymbol, HashSet<object?>>(TypeSymbolReferenceComparer.Instance);
                coverage[member] = new HashSet<object?>();
            }
        }

        return coverage;
    }

    private static bool TryUpdateLiteralCoverage(
        HashSet<TypeSymbol> remaining,
        Dictionary<TypeSymbol, HashSet<object?>>? literalCoverage,
        LiteralTypeSymbol literal)
    {
        if (literalCoverage is null || literalCoverage.Count == 0)
            return false;

        var updated = false;

        foreach (var entry in literalCoverage.ToArray())
        {
            var candidate = entry.Key;

            if (!remaining.Contains(candidate))
            {
                literalCoverage.Remove(candidate);
                continue;
            }

            var targetType = UnwrapAlias(candidate);

            if (!TypeCoverageHelper.LiteralBelongsToType(literal, targetType))
                continue;

            updated = true;

            var constants = entry.Value;
            constants.Add(literal.ConstantValue);

            if (TypeCoverageHelper.LiteralsCoverType(targetType, constants))
            {
                remaining.Remove(candidate);
                literalCoverage.Remove(candidate);
            }
        }

        return updated;
    }

    private static bool CanBeNull(TypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type.TypeKind == TypeKind.Error)
            return true;

        if (type is NullTypeSymbol)
            return true;

        if (type.IsNullable)
            return true;

        if (type.TryGetUnion() is { ContentMayBeNull: true })
            return true;

        return false;
    }

    private bool IsAssignable(TypeSymbol targetType, TypeSymbol sourceType)
    {
        if (targetType.ContainsErrorType() || sourceType.ContainsErrorType())
            return true;

        var conversion = _compilation.ClassifyConversion(sourceType, targetType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static ImmutableArray<TypeSymbol> GetTupleElementTypes(TypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type is NamedTypeSymbol { IsTupleType: true } namedTuple)
        {
            var elements = namedTuple.TupleElements;
            if (!elements.IsDefaultOrEmpty)
                return elements.Select(e => e.Type).ToImmutableArray();
        }

        if (type is TupleTypeSymbol tuple)
        {
            return tuple.TupleElements.Select(e => e.Type).ToImmutableArray();
        }

        return ImmutableArray<TypeSymbol>.Empty;
    }

    private static bool IsTotalRestOnlySequencePattern(
        TypeSymbol inputType,
        BoundPositionalPattern pattern)
    {
        if (GetTupleElementTypes(inputType).Length > 0 ||
            pattern.RestIndex != 0 ||
            pattern.Elements.Length != 1 ||
            pattern.ElementKinds.Length != 1 ||
            pattern.ElementKinds[0] != BoundPositionalPattern.SequenceElementKind.RestSegment)
        {
            return false;
        }

        return IsCatchAllPattern(pattern.Elements[0].Type, pattern.Elements[0]);
    }

    private static bool AreSameUnionPatternTarget(TypeSymbol left, TypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        if (left is not NamedTypeSymbol leftNamed || right is not NamedTypeSymbol rightNamed)
            return false;

        leftNamed = leftNamed.OriginalDefinition as NamedTypeSymbol ?? leftNamed;
        rightNamed = rightNamed.OriginalDefinition as NamedTypeSymbol ?? rightNamed;

        if (!string.Equals(leftNamed.Name, rightNamed.Name, StringComparison.Ordinal))
            return false;

        if (leftNamed.TypeParameters.Length != rightNamed.TypeParameters.Length)
            return false;

        var leftNs = leftNamed.ContainingNamespace?.ToDisplayString();
        var rightNs = rightNamed.ContainingNamespace?.ToDisplayString();
        return string.Equals(leftNs, rightNs, StringComparison.Ordinal);
    }

    private static bool ArePatternTypesEquivalent(TypeSymbol left, TypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        left = UnwrapAlias(left);
        right = UnwrapAlias(right);

        if (left.SpecialType != SpecialType.None && left.SpecialType == right.SpecialType)
            return true;

        if (left is NamedTypeSymbol leftNamed && right is NamedTypeSymbol rightNamed)
        {
            leftNamed = leftNamed.OriginalDefinition as NamedTypeSymbol ?? leftNamed;
            rightNamed = rightNamed.OriginalDefinition as NamedTypeSymbol ?? rightNamed;

            if (!string.Equals(leftNamed.Name, rightNamed.Name, StringComparison.Ordinal))
                return false;

            if (leftNamed.TypeParameters.Length != rightNamed.TypeParameters.Length)
                return false;

            var leftNs = leftNamed.ContainingNamespace?.ToDisplayString();
            var rightNs = rightNamed.ContainingNamespace?.ToDisplayString();
            return string.Equals(leftNs, rightNs, StringComparison.Ordinal);
        }

        return false;
    }

    private sealed class TypeSymbolReferenceComparer : IEqualityComparer<TypeSymbol>
    {
        public static TypeSymbolReferenceComparer Instance { get; } = new();

        public bool Equals(TypeSymbol? x, TypeSymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(TypeSymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }

    private sealed class SymbolReferenceComparer<TSymbol> : IEqualityComparer<TSymbol>
        where TSymbol : class, Symbol
    {
        public static SymbolReferenceComparer<TSymbol> Instance { get; } = new();

        public bool Equals(TSymbol? x, TSymbol? y) => ReferenceEquals(x, y);

        public int GetHashCode(TSymbol obj) => RuntimeHelpers.GetHashCode(obj);
    }

    private static TypeSymbol UnwrapAlias(TypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is TypeSymbol alias)
            type = alias;

        return type;
    }

    // =========================================================
    // Numeric interval coverage (for range/comparison patterns)
    // =========================================================

    internal ImmutableArray<string> GetMissingNumericRangeCases(
        TypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        NumericInterval domain,
        MatchExhaustivenessOptions options)
    {
        // Start with the full domain as the uncovered region.
        var coverage = new NumericMatchCoverage(domain);

        foreach (var arm in arms)
        {
            if (coverage.IsComplete)
                break;

            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            // If any arm's pattern cannot be expressed as intervals, fall back to
            // catch-all check (existing behavior).
            if (!TryGetPatternIntervals(arm.Pattern, domain, out var covered))
            {
                // If catch-all, return empty (exhaustive)
                if (IsCatchAllPattern(scrutineeType, arm.Pattern))
                    return ImmutableArray<string>.Empty;

                // Otherwise, we can't reason — fall back to requiring a catch-all.
                var hasCatchAll = arms.Any(a => a.Guard is null && IsCatchAllPattern(scrutineeType, a.Pattern));
                var hasCatchAllIgnoringOption = hasCatchAll && !options.IgnoreCatchAllPatterns;
                return hasCatchAllIgnoringOption ? ImmutableArray<string>.Empty : ImmutableArray.Create("_");
            }

            foreach (var interval in covered)
                coverage.Cover(interval);
        }

        return coverage.GetMissingCases(scrutineeType.SpecialType == SpecialType.System_Char);
    }

    /// <summary>
    /// Tries to extract a list of numeric intervals that a pattern covers, given a domain.
    /// Returns false if the pattern cannot be expressed as intervals (e.g., it's a type pattern).
    /// </summary>
    private static bool TryGetPatternIntervals(
        BoundPattern pattern,
        NumericInterval domain,
        out IReadOnlyList<NumericInterval> intervals)
    {
        intervals = Array.Empty<NumericInterval>();

        switch (pattern)
        {
            case BoundGuardedPattern guarded:
                {
                    if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(guarded.GuardExpression) ||
                        !TryGetPatternIntervals(guarded.Pattern, domain, out var patternIntervals))
                    {
                        return false;
                    }

                    if (guarded.GuardPattern is null)
                    {
                        intervals = patternIntervals;
                        return true;
                    }

                    if (!TryGetPatternIntervals(guarded.GuardPattern, domain, out var guardIntervals))
                        return false;

                    var guardedIntersection = new List<NumericInterval>();
                    foreach (var patternInterval in patternIntervals)
                    {
                        foreach (var guardInterval in guardIntervals)
                        {
                            var low = Int128.Max(patternInterval.Low, guardInterval.Low);
                            var high = Int128.Min(patternInterval.High, guardInterval.High);
                            if (low <= high)
                                guardedIntersection.Add(new NumericInterval(low, high));
                        }
                    }

                    intervals = guardedIntersection;
                    return true;
                }
            case BoundDiscardPattern:
            case BoundDeclarationPattern { Designator: BoundDiscardDesignator }:
                intervals = new[] { domain };
                return true;

            case BoundRangePattern range:
                {
                    var low = domain.Low;
                    if (range.LowerBound is not null &&
                        !TryExtractNumericValue(range.LowerBound, out low))
                    {
                        return false;
                    }

                    var high = domain.High;

                    if (range.UpperBound is not null)
                    {
                        if (!TryExtractNumericValue(range.UpperBound, out var hi))
                            return false;
                        high = range.IsUpperExclusive ? hi - 1 : hi;
                    }

                    intervals = new[] { new NumericInterval(Int128.Max(low, domain.Low), Int128.Min(high, domain.High)) };
                    return true;
                }

            case BoundComparisonPattern comparison:
                {
                    if (!TryExtractNumericValue(comparison.Value, out var v))
                        return false;

                    if (comparison.Operator == BoundComparisonPatternOperator.NotEquals)
                        return false;

                    var interval = comparison.Operator switch
                    {
                        BoundComparisonPatternOperator.Equals => new NumericInterval(v, v),
                        BoundComparisonPatternOperator.GreaterThan => new NumericInterval(v + 1, domain.High),
                        BoundComparisonPatternOperator.GreaterThanOrEqual => new NumericInterval(v, domain.High),
                        BoundComparisonPatternOperator.LessThan => new NumericInterval(domain.Low, v - 1),
                        BoundComparisonPatternOperator.LessThanOrEqual => new NumericInterval(domain.Low, v),
                        _ => default
                    };
                    // Clamp to domain
                    if (interval.Low > interval.High || interval.Low > domain.High || interval.High < domain.Low)
                        return true; // empty interval (valid but covers nothing)
                    intervals = new[] { new NumericInterval(Int128.Max(interval.Low, domain.Low), Int128.Min(interval.High, domain.High)) };
                    return true;
                }

            case BoundConstantPattern constant:
                {
                    if (!NumericInterval.TryGetValue(constant.ConstantValue, out var v))
                        return false;
                    if (v < domain.Low || v > domain.High)
                        return true; // out of domain, covers nothing
                    intervals = new[] { new NumericInterval(v, v) };
                    return true;
                }

            case BoundOrPattern or:
                {
                    if (!TryGetPatternIntervals(or.Left, domain, out var leftIntervals))
                        return false;
                    if (!TryGetPatternIntervals(or.Right, domain, out var rightIntervals))
                        return false;
                    var combined = new List<NumericInterval>(leftIntervals);
                    combined.AddRange(rightIntervals);
                    intervals = combined;
                    return true;
                }

            case BoundAndPattern and:
                {
                    if (!TryGetPatternIntervals(and.Left, domain, out var leftIntervals))
                        return false;
                    if (!TryGetPatternIntervals(and.Right, domain, out var rightIntervals))
                        return false;
                    // Intersection of left and right interval sets
                    var intersection = new List<NumericInterval>();
                    foreach (var l in leftIntervals)
                        foreach (var r in rightIntervals)
                        {
                            var lo = Int128.Max(l.Low, r.Low);
                            var hi = Int128.Min(l.High, r.High);
                            if (lo <= hi)
                                intersection.Add(new NumericInterval(lo, hi));
                        }
                    intervals = intersection;
                    return true;
                }

            case BoundNotPattern not:
                {
                    if (!TryGetPatternIntervals(not.Pattern, domain, out var inner))
                        return false;
                    // Complement: start from domain and subtract each inner interval
                    var complement = new List<NumericInterval> { domain };
                    foreach (var i in inner)
                    {
                        var next = new List<NumericInterval>();
                        foreach (var c in complement)
                            next.AddRange(c.Subtract(i));
                        complement = next;
                    }
                    intervals = complement;
                    return true;
                }

            default:
                return false;
        }
    }

    private static bool TryExtractNumericValue(BoundExpression expr, out Int128 value)
        => NumericInterval.TryGetValue(expr switch
        {
            BoundLiteralExpression literal => literal.Value,
            BoundConstantPattern constant => constant.ConstantValue,
            BoundFieldAccess field => field.Field.GetConstantValue(),
            BoundMemberAccessExpression { Member: FieldSymbol field } => field.GetConstantValue(),
            _ => null
        }, out value);

    [Flags]
    private enum BooleanCoverage : byte
    {
        None = 0,
        False = 1,
        True = 2,
        All = False | True,
    }

    private enum TypePatternCoverage : byte
    {
        None,
        Some,
        All,
    }
}
