using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class MatchExhaustivenessEvaluator
{
    private readonly Compilation _compilation;

    public MatchExhaustivenessEvaluator(Compilation compilation)
    {
        _compilation = compilation;
    }

    public MatchExhaustivenessInfo Evaluate(BoundMatchExpression matchExpression, MatchExhaustivenessOptions options)
    {
        if (matchExpression.Expression.Type is not ITypeSymbol scrutineeType)
            return new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false);

        scrutineeType = UnwrapAlias(scrutineeType);

        if (scrutineeType.TypeKind == TypeKind.Error)
            return new MatchExhaustivenessInfo(isExhaustive: true, ImmutableArray<string>.Empty, hasCatchAll: false);

        var arms = matchExpression.Arms;
        var hasCatchAll = arms.Any(arm => arm.Guard is null && IsCatchAllPattern(scrutineeType, arm.Pattern));

        ImmutableArray<string> missingCases;

        if (IsBooleanType(scrutineeType))
        {
            missingCases = GetMissingBooleanCases(scrutineeType, arms, options);
        }
        else
        {
            var discriminatedUnion = scrutineeType.TryGetDiscriminatedUnion()
                ?? scrutineeType.TryGetDiscriminatedUnionCase()?.Union;

            if (discriminatedUnion is not null)
            {
                missingCases = GetMissingDiscriminatedUnionCases(scrutineeType, arms, discriminatedUnion, options);
            }
            else if (scrutineeType is INamedTypeSymbol { TypeKind: TypeKind.Enum } enumType)
            {
                missingCases = GetMissingEnumCases(scrutineeType, arms, enumType, options);
            }
            else if (scrutineeType is ITypeUnionSymbol typeUnion)
            {
                missingCases = GetMissingUnionCases(scrutineeType, arms, typeUnion, options);
            }
            else
            {
                var hasCatchAllIgnoringOption = hasCatchAll && !options.IgnoreCatchAllPatterns;
                missingCases = hasCatchAllIgnoringOption ? ImmutableArray<string>.Empty : ImmutableArray.Create("_");
            }
        }

        return new MatchExhaustivenessInfo(missingCases.IsDefaultOrEmpty, missingCases, hasCatchAll);
    }

    internal static bool IsDiscardPattern(BoundPattern pattern)
    {
        return pattern is BoundDiscardPattern ||
            pattern is BoundDeclarationPattern { Designator: BoundDiscardDesignator };
    }

    internal static bool IsCatchAllPattern(ITypeSymbol scrutineeType, BoundPattern pattern)
    {
        if (pattern is BoundConstantPattern || pattern is BoundRelationalPattern)
            return false;

        switch (pattern)
        {
            case BoundDiscardPattern:
                return true;
            case BoundDeclarationPattern { Designator: BoundDiscardDesignator } declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (SymbolEqualityComparer.Default.Equals(declaredType, scrutineeType))
                        return true;

                    return declaredType.SpecialType == SpecialType.System_Object;
                }
            case BoundOrPattern orPattern:
                return IsCatchAllPattern(scrutineeType, orPattern.Left) ||
                       IsCatchAllPattern(scrutineeType, orPattern.Right);
            case BoundTuplePattern tuplePattern:
                {
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
        ITypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        MatchExhaustivenessOptions options)
    {
        var remaining = BooleanCoverage.All;

        foreach (var arm in arms)
        {
            if (arm.Guard is not null)
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

    private ImmutableArray<string> GetMissingDiscriminatedUnionCases(
        ITypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        IDiscriminatedUnionSymbol union,
        MatchExhaustivenessOptions options)
    {
        var remaining = new HashSet<IDiscriminatedUnionCaseSymbol>(union.Cases, SymbolEqualityComparer.Default);

        foreach (var arm in arms)
        {
            if (!BoundNodeFacts.MatchArmGuardGuaranteesMatch(arm.Guard))
                continue;

            if (options.IgnoreCatchAllPatterns && IsCatchAllPattern(scrutineeType, arm.Pattern))
                continue;

            RemoveCoveredCases(remaining, arm.Pattern, union);

            if (remaining.Count == 0)
                break;
        }

        return remaining
            .Select(caseSymbol => caseSymbol.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat))
            .OrderBy(name => name, StringComparer.Ordinal)
            .ToImmutableArray();
    }

    private ImmutableArray<string> GetMissingEnumCases(
        ITypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        INamedTypeSymbol enumType,
        MatchExhaustivenessOptions options)
    {
        var remaining = new HashSet<IFieldSymbol>(GetEnumMembers(enumType), SymbolEqualityComparer.Default);

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

    private ImmutableArray<string> GetMissingUnionCases(
        ITypeSymbol scrutineeType,
        ImmutableArray<BoundMatchArm> arms,
        ITypeUnionSymbol union,
        MatchExhaustivenessOptions options)
    {
        var remaining = new HashSet<ITypeSymbol>(GetUnionMembers(union), SymbolEqualityComparer.Default);
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

    private ImmutableArray<string> GetMissingLiteralCoverage(ITypeSymbol type, HashSet<object?> constants)
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

    private bool IsTotalPattern(ITypeSymbol inputType, BoundPattern pattern)
    {
        inputType = UnwrapAlias(inputType);

        switch (pattern)
        {
            case BoundDiscardPattern:
                return true;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (declaredType.TypeKind == TypeKind.Error || inputType.TypeKind == TypeKind.Error)
                        return true;

                    return IsAssignable(declaredType, inputType);
                }
            case BoundTuplePattern tuplePattern:
                {
                    var elementTypes = GetTupleElementTypes(inputType);

                    if (elementTypes.Length == 0 || elementTypes.Length != tuplePattern.Elements.Length)
                        return false;

                    for (var i = 0; i < tuplePattern.Elements.Length; i++)
                    {
                        if (!IsTotalPattern(elementTypes[i], tuplePattern.Elements[i]))
                            return false;
                    }

                    return true;
                }
            case BoundPropertyPattern propertyPattern:
                {
                    if (CanBeNull(inputType))
                        return false;

                    if (propertyPattern.NarrowedType is not null &&
                        !IsAssignable(propertyPattern.NarrowedType, inputType))
                    {
                        return false;
                    }

                    foreach (var property in propertyPattern.Properties)
                    {
                        if (!IsTotalPattern(property.Type, property.Pattern))
                            return false;
                    }

                    return true;
                }
            case BoundOrPattern orPattern:
                return IsTotalPattern(inputType, orPattern.Left) ||
                       IsTotalPattern(inputType, orPattern.Right);
            case BoundAndPattern andPattern:
                return IsTotalPattern(inputType, andPattern.Left) &&
                       IsTotalPattern(inputType, andPattern.Right);
            default:
                return false;
        }
    }

    private static bool TryGetLiteralBoolConstant(BoundPattern pattern, out bool value)
    {
        value = default;

        if (pattern is not BoundConstantPattern constant)
            return false;

        if (constant.Expression is not null)
            return false;

        if (constant.LiteralType is not LiteralTypeSymbol literal)
            return false;

        if (literal.ConstantValue is not bool b)
            return false;

        value = b;
        return true;
    }

    private void RemoveCoveredUnionMembers(
        HashSet<ITypeSymbol> remaining,
        BoundPattern pattern,
        Dictionary<ITypeSymbol, HashSet<object?>>? literalCoverage = null)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                remaining.Clear();
                literalCoverage?.Clear();
                break;
            case BoundDeclarationPattern declaration:
                RemoveMembersAssignableToPattern(remaining, declaration.DeclaredType, literalCoverage);
                break;
            case BoundConstantPattern constant:
                {
                    if (constant.Expression is not null)
                        break;

                    if (constant.LiteralType is null)
                        break;

                    var literalType = (LiteralTypeSymbol)UnwrapAlias(constant.LiteralType);

                    if (TryUpdateLiteralCoverage(remaining, literalCoverage, literalType))
                        break;

                    if (constant.ConstantValue is null)
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

                        break;
                    }

                    foreach (var candidate in remaining.ToArray())
                    {
                        var candidateType = UnwrapAlias(candidate);

                        if (SymbolEqualityComparer.Default.Equals(candidateType, literalType))
                        {
                            remaining.Remove(candidate);
                            literalCoverage?.Remove(candidate);
                        }
                    }

                    break;
                }
            case BoundOrPattern orPattern:
                RemoveCoveredUnionMembers(remaining, orPattern.Left, literalCoverage);
                RemoveCoveredUnionMembers(remaining, orPattern.Right, literalCoverage);
                break;
            case BoundTuplePattern tuplePattern:
                RemoveMembersAssignableToPattern(remaining, tuplePattern.Type, literalCoverage);
                break;
        }
    }

    private void RemoveCoveredCases(
        HashSet<IDiscriminatedUnionCaseSymbol> remaining,
        BoundPattern pattern,
        IDiscriminatedUnionSymbol union)
    {
        switch (pattern)
        {
            case BoundDiscardPattern:
                remaining.Clear();
                break;
            case BoundDeclarationPattern declaration:
                {
                    var declaredType = UnwrapAlias(declaration.DeclaredType);

                    if (declaredType.SpecialType == SpecialType.System_Object ||
                        SymbolEqualityComparer.Default.Equals(declaredType, UnwrapAlias((ITypeSymbol)union)))
                    {
                        remaining.Clear();
                        break;
                    }

                    var declarationUnion = declaredType.TryGetDiscriminatedUnion()
                        ?? declaredType.TryGetDiscriminatedUnionCase()?.Union;

                    if (declarationUnion is not null &&
                        SymbolEqualityComparer.Default.Equals(UnwrapAlias(declarationUnion), UnwrapAlias(union)))
                    {
                        remaining.Clear();
                    }

                    break;
                }
            case BoundCasePattern casePattern:
                if (SymbolEqualityComparer.Default.Equals(UnwrapAlias(casePattern.CaseSymbol.Union), UnwrapAlias(union)) &&
                    CasePatternCoversAllArguments(casePattern))
                {
                    remaining.Remove(casePattern.CaseSymbol);
                }

                break;
            case BoundOrPattern orPattern:
                RemoveCoveredCases(remaining, orPattern.Left, union);
                RemoveCoveredCases(remaining, orPattern.Right, union);
                break;
        }
    }

    private static IEnumerable<IFieldSymbol> GetEnumMembers(INamedTypeSymbol enumType)
    {
        var normalizedEnum = (INamedTypeSymbol)UnwrapAlias(enumType);

        return normalizedEnum
            .GetMembers()
            .OfType<IFieldSymbol>()
            .Where(field =>
                field.IsConst &&
                SymbolEqualityComparer.Default.Equals(UnwrapAlias(field.Type), normalizedEnum));
    }

    private void RemoveCoveredEnumMembers(HashSet<IFieldSymbol> remaining, INamedTypeSymbol enumType, BoundPattern pattern)
    {
        enumType = (INamedTypeSymbol)UnwrapAlias(enumType);

        if (IsCatchAllPattern(enumType, pattern))
        {
            remaining.Clear();
            return;
        }

        switch (pattern)
        {
            case BoundConstantPattern constant:
                if (constant.Expression is BoundFieldAccess fieldAccess &&
                    fieldAccess.Field.IsConst &&
                    SymbolEqualityComparer.Default.Equals(UnwrapAlias(fieldAccess.Field.Type), enumType))
                {
                    remaining.Remove(fieldAccess.Field);
                }

                break;
            case BoundOrPattern orPattern:
                RemoveCoveredEnumMembers(remaining, enumType, orPattern.Left);
                RemoveCoveredEnumMembers(remaining, enumType, orPattern.Right);
                break;
        }
    }

    private bool CasePatternCoversAllArguments(BoundCasePattern casePattern)
    {
        var parameters = casePattern.CaseSymbol.ConstructorParameters;
        var argumentCount = Math.Min(parameters.Length, casePattern.Arguments.Length);

        for (var i = 0; i < argumentCount; i++)
        {
            if (!IsTotalPattern(parameters[i].Type, casePattern.Arguments[i]))
                return false;
        }

        return true;
    }

    private void RemoveMembersAssignableToPattern(
        HashSet<ITypeSymbol> remaining,
        ITypeSymbol patternType,
        Dictionary<ITypeSymbol, HashSet<object?>>? literalCoverage = null)
    {
        patternType = UnwrapAlias(patternType);

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

            if (IsAssignable(patternType, candidateType))
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
                continue;
            }

            if (patternType is ITypeUnionSymbol patternUnion &&
                candidateType is ITypeUnionSymbol candidateUnion &&
                TypeCoverageHelper.UnionIsCoveredByTypes(patternUnion, candidateUnion.Types))
            {
                remaining.Remove(candidate);
                literalCoverage?.Remove(candidate);
            }
        }
    }

    private static IEnumerable<ITypeSymbol> GetUnionMembers(ITypeUnionSymbol union)
    {
        foreach (var member in union.Types)
        {
            if (member is ITypeUnionSymbol nested)
            {
                foreach (var nestedMember in GetUnionMembers(nested))
                    yield return UnwrapAlias(nestedMember);
            }
            else
            {
                yield return UnwrapAlias(member);
            }
        }
    }

    private static bool IsBooleanType(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type.SpecialType == SpecialType.System_Boolean)
            return true;

        if (type is LiteralTypeSymbol literal)
            return IsBooleanType(literal.UnderlyingType);

        return false;
    }

    private static Dictionary<ITypeSymbol, HashSet<object?>>? CreateLiteralCoverage(IEnumerable<ITypeSymbol> members)
    {
        Dictionary<ITypeSymbol, HashSet<object?>>? coverage = null;

        foreach (var member in members)
        {
            var type = UnwrapAlias(member);

            if (TypeCoverageHelper.RequiresLiteralCoverage(type))
            {
                coverage ??= new Dictionary<ITypeSymbol, HashSet<object?>>(SymbolEqualityComparer.Default);
                coverage[member] = new HashSet<object?>();
            }
        }

        return coverage;
    }

    private static bool TryUpdateLiteralCoverage(
        HashSet<ITypeSymbol> remaining,
        Dictionary<ITypeSymbol, HashSet<object?>>? literalCoverage,
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

    private static bool CanBeNull(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type.TypeKind == TypeKind.Error)
            return true;

        if (type is NullTypeSymbol)
            return true;

        if (type.IsNullable)
            return true;

        if (type is ITypeUnionSymbol union)
        {
            foreach (var member in union.Types)
            {
                if (CanBeNull(member))
                    return true;
            }
        }

        return false;
    }

    private bool IsAssignable(ITypeSymbol targetType, ITypeSymbol sourceType)
    {
        if (targetType.ContainsErrorType() || sourceType.ContainsErrorType())
            return true;

        var conversion = _compilation.ClassifyConversion(sourceType, targetType);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static ImmutableArray<ITypeSymbol> GetTupleElementTypes(ITypeSymbol type)
    {
        type = UnwrapAlias(type);

        if (type is INamedTypeSymbol { IsTupleType: true } namedTuple)
        {
            var elements = namedTuple.TupleElements;
            if (!elements.IsDefaultOrEmpty)
                return elements.Select(e => e.Type).ToImmutableArray();
        }

        if (type is ITupleTypeSymbol tuple)
        {
            return tuple.TupleElements.Select(e => e.Type).ToImmutableArray();
        }

        return ImmutableArray<ITypeSymbol>.Empty;
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            type = alias;

        return type;
    }

    [Flags]
    private enum BooleanCoverage : byte
    {
        None = 0,
        False = 1,
        True = 2,
        All = False | True,
    }
}
