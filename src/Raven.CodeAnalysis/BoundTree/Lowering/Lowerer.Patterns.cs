using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private BoundPattern RewritePatternForMatch(
        BoundPattern pattern,
        ITypeSymbol? inputType,
        Compilation compilation)
    {
        pattern = pattern switch
        {
            BoundGuardedPattern guardedPattern => RewriteGuardedPatternForMatch(guardedPattern, inputType, compilation),
            BoundAndPattern andPattern => new BoundAndPattern(
                RewritePatternForMatch(andPattern.Left, inputType, compilation),
                RewritePatternForMatch(andPattern.Right, inputType, compilation)),
            BoundOrPattern orPattern => new BoundOrPattern(
                RewritePatternForMatch(orPattern.Left, inputType, compilation),
                RewritePatternForMatch(orPattern.Right, inputType, compilation)),
            BoundNotPattern notPattern => new BoundNotPattern(
                RewritePatternForMatch(notPattern.Pattern, inputType, compilation)),
            BoundUnionMemberPattern unionMemberPattern => RewriteUnionMemberPatternForMatch(unionMemberPattern, compilation),
            _ => RewriteNullDiscardPattern(pattern, compilation)
        };

        return TryRewriteUnionConstantPattern(pattern, inputType, out var rewrittenPattern)
            ? rewrittenPattern
            : pattern;
    }

    private BoundPattern RewriteGuardedPatternForMatch(
        BoundGuardedPattern guardedPattern,
        ITypeSymbol? inputType,
        Compilation compilation)
    {
        var pattern = RewritePatternForMatch(guardedPattern.Pattern, inputType, compilation);
        var guardPattern = guardedPattern.GuardPattern is null
            ? null
            : RewritePatternForMatch(guardedPattern.GuardPattern, inputType, compilation);
        var guardExpression = guardedPattern.GuardExpression is null
            ? null
            : (BoundExpression?)VisitExpression(guardedPattern.GuardExpression);

        return new BoundGuardedPattern(pattern, guardPattern, guardExpression, guardedPattern.Reason);
    }

    private BoundPattern RewriteUnionMemberPatternForMatch(
        BoundUnionMemberPattern unionMemberPattern,
        Compilation compilation)
    {
        var pattern = RewritePatternForMatch(unionMemberPattern.Pattern, unionMemberPattern.MemberType, compilation);
        return ReferenceEquals(pattern, unionMemberPattern.Pattern)
            ? unionMemberPattern
            : new BoundUnionMemberPattern(
                unionMemberPattern.UnionType,
                unionMemberPattern.MemberType,
                unionMemberPattern.TryGetMethod,
                pattern,
                unionMemberPattern.Reason);
    }

    private static BoundPattern RewriteNullDiscardPattern(BoundPattern pattern, Compilation compilation)
    {
        if (pattern is BoundDeclarationPattern
            {
                Type: NullTypeSymbol,
                Designator: BoundDiscardDesignator
            } declarationPattern)
        {
            var nullLiteral = new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, compilation.NullTypeSymbol);
            return new BoundConstantPattern(nullLiteral, reason: declarationPattern.Reason);
        }

        return pattern;
    }

    private bool TryRewriteUnionConstantPattern(
        BoundPattern pattern,
        ITypeSymbol? inputType,
        out BoundPattern rewrittenPattern)
    {
        rewrittenPattern = pattern;

        if (pattern is not BoundConstantPattern constantPattern || inputType is null)
            return false;

        var unionType = inputType.TryGetUnion()
            ?? inputType.TryGetUnionCase()?.Union;

        if (unionType is null || unionType.MemberTypes.IsDefaultOrEmpty)
            return false;

        var patternType = GetConstantPatternComparisonType(constantPattern);
        if (patternType is null || patternType.TypeKind == TypeKind.Error)
            return false;

        var matches = ImmutableArray.CreateBuilder<BoundPattern>();
        foreach (var memberType in unionType.MemberTypes)
        {
            if (!CanUnionMemberMatchConstant(memberType, patternType))
                continue;

            var tryGetMethod = FindTryGetMethod(inputType, unionType, memberType);
            if (tryGetMethod is null)
                continue;

            matches.Add(new BoundUnionMemberPattern(inputType, memberType, tryGetMethod, constantPattern));
        }

        if (matches.Count == 0)
            return false;

        rewrittenPattern = matches[0];
        for (var i = 1; i < matches.Count; i++)
            rewrittenPattern = new BoundOrPattern(rewrittenPattern, matches[i]);

        return true;
    }

    private static ITypeSymbol? GetConstantPatternComparisonType(BoundConstantPattern constantPattern)
    {
        if (constantPattern.Type.TypeKind == TypeKind.Null)
            return constantPattern.Type;

        if (constantPattern.LiteralType is not null)
            return constantPattern.LiteralType.UnderlyingType;

        return constantPattern.Expression?.Type;
    }

    private static bool CanUnionMemberMatchConstant(ITypeSymbol memberType, ITypeSymbol patternType)
    {
        var memberContentType = UnionContentNullability.GetNonNullContentType(memberType, out var memberMayBeNull);
        var patternContentType = UnionContentNullability.GetNonNullContentType(patternType, out _);

        if (patternContentType.TypeKind == TypeKind.Null)
            return memberMayBeNull || memberContentType.TypeKind == TypeKind.Null;

        return AreSameUnionMemberPatternTarget(memberContentType, patternContentType);
    }

    private static IMethodSymbol? FindTryGetMethod(
        ITypeSymbol? lookupType,
        IUnionSymbol unionType,
        ITypeSymbol targetType)
    {
        var targetCaseType = targetType.GetPlainType();
        var targetUnion = (INamedTypeSymbol)UnwrapAlias((INamedTypeSymbol)unionType);
        var targetUnionCase = targetType.TryGetUnionCase();

        bool MatchesTargetCase(IMethodSymbol method)
        {
            if (method.IsStatic || method.Parameters.Length != 1)
                return false;

            var parameter = method.Parameters[0];
            if (parameter.RefKind is not (RefKind.Out or RefKind.Ref))
                return false;

            var parameterType = parameter.GetByRefElementType().GetPlainType();
            if (parameterType.MetadataIdentityEquals(targetCaseType))
                return true;

            var parameterCase = parameterType.TryGetUnionCase();
            if (parameterCase is not null)
            {
                var parameterUnion = (INamedTypeSymbol)UnwrapAlias((INamedTypeSymbol)parameterCase.Union);
                return targetUnionCase is not null &&
                    parameterCase.Ordinal == targetUnionCase.Ordinal &&
                    AreSameUnionPatternTarget(parameterUnion, targetUnion);
            }

            return SymbolEqualityComparer.Default.Equals(parameterType, targetCaseType);
        }

        var candidateTypes = new List<INamedTypeSymbol>();

        void AddCandidate(INamedTypeSymbol? candidate)
        {
            if (candidate is null)
                return;

            candidate = (INamedTypeSymbol)UnwrapAlias(candidate);

            if (candidateTypes.Any(t => SymbolEqualityComparer.Default.Equals(t, candidate)))
                return;

            candidateTypes.Add(candidate);

            if (candidate.ConstructedFrom is INamedTypeSymbol constructedFrom &&
                !SymbolEqualityComparer.Default.Equals(constructedFrom, candidate))
            {
                AddCandidate(constructedFrom);
            }
        }

        AddCandidate(unionType as INamedTypeSymbol);
        AddCandidate(targetType as INamedTypeSymbol);

        if (lookupType is INamedTypeSymbol namedLookup)
            AddCandidate(namedLookup);

        foreach (var candidate in candidateTypes)
        {
            var method = candidate
                .GetMembers("TryGetValue")
                .OfType<IMethodSymbol>()
                .FirstOrDefault(MatchesTargetCase);

            if (method is not null)
                return method;
        }

        return null;
    }

    private static bool AreSameUnionMemberPatternTarget(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        var leftPlain = left.GetPlainType();
        var rightPlain = right.GetPlainType();
        if (SymbolEqualityComparer.Default.Equals(leftPlain, rightPlain))
            return true;

        var leftDefinition = left.OriginalDefinition ?? left;
        var rightDefinition = right.OriginalDefinition ?? right;
        return SymbolEqualityComparer.Default.Equals(leftDefinition, rightDefinition);
    }

    private static bool AreSameUnionPatternTarget(ITypeSymbol left, ITypeSymbol right)
    {
        if (ReferenceEquals(left, right))
            return true;

        if (left is not INamedTypeSymbol leftNamed || right is not INamedTypeSymbol rightNamed)
            return false;

        leftNamed = leftNamed.OriginalDefinition as INamedTypeSymbol ?? leftNamed;
        rightNamed = rightNamed.OriginalDefinition as INamedTypeSymbol ?? rightNamed;

        if (!string.Equals(leftNamed.Name, rightNamed.Name, StringComparison.Ordinal))
            return false;

        if (leftNamed.TypeParameters.Length != rightNamed.TypeParameters.Length)
            return false;

        var leftNs = leftNamed.ContainingNamespace?.ToDisplayString();
        var rightNs = rightNamed.ContainingNamespace?.ToDisplayString();
        return string.Equals(leftNs, rightNs, StringComparison.Ordinal);
    }

    private static ITypeSymbol UnwrapAlias(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            type = alias;

        return type;
    }
}
