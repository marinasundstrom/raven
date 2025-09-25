using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class OverloadResolver
{
    public static OverloadResolutionResult ResolveOverload(
        IEnumerable<IMethodSymbol> methods,
        BoundExpression[] arguments,
        Compilation compilation,
        BoundExpression? receiver = null)
    {
        IMethodSymbol? bestMatch = null;
        int bestScore = int.MaxValue;
        ImmutableArray<IMethodSymbol>.Builder? ambiguous = null;
        bool bestIsExtension = false;

        foreach (var method in methods)
        {
            var parameters = method.Parameters;
            var treatAsExtension = method.IsExtensionMethod && receiver is not null;
            var expectedParameters = treatAsExtension ? arguments.Length + 1 : arguments.Length;

            if (parameters.Length != expectedParameters)
                continue;

            if (!TryMatch(method, arguments, receiver, treatAsExtension, compilation, out var score))
                continue;

            if (score < bestScore)
            {
                bestMatch = method;
                bestScore = score;
                ambiguous = null;
                bestIsExtension = treatAsExtension;
                continue;
            }

            if (score > bestScore || bestMatch is null)
                continue;

            if (bestIsExtension != treatAsExtension)
            {
                if (!treatAsExtension)
                {
                    bestMatch = method;
                    bestIsExtension = false;
                    ambiguous = null;
                }
                continue;
            }

            if (IsMoreSpecific(method, bestMatch, arguments, receiver, compilation))
            {
                bestMatch = method;
                ambiguous = null;
                bestIsExtension = treatAsExtension;
                continue;
            }

            if (IsMoreSpecific(bestMatch, method, arguments, receiver, compilation))
                continue;

            ambiguous ??= ImmutableArray.CreateBuilder<IMethodSymbol>();
            AddCandidateIfMissing(ambiguous, bestMatch);
            AddCandidateIfMissing(ambiguous, method);
        }

        if (ambiguous is { Count: > 0 })
            return OverloadResolutionResult.Ambiguous(ambiguous.ToImmutable());

        return new OverloadResolutionResult(bestMatch);
    }

    private static void AddCandidateIfMissing(ImmutableArray<IMethodSymbol>.Builder builder, IMethodSymbol candidate)
    {
        foreach (var existing in builder)
        {
            if (SymbolEqualityComparer.Default.Equals(existing, candidate))
                return;
        }

        builder.Add(candidate);
    }

    private static bool IsMoreSpecific(
        IMethodSymbol candidate,
        IMethodSymbol current,
        BoundExpression[] arguments,
        BoundExpression? receiver,
        Compilation compilation)
    {
        bool better = false;
        var candParams = candidate.Parameters;
        var currentParams = current.Parameters;

        bool candidateIsExtension = candidate.IsExtensionMethod && receiver is not null;
        bool currentIsExtension = current.IsExtensionMethod && receiver is not null;

        if (candidateIsExtension != currentIsExtension)
            return !currentIsExtension;

        int candParamIndex = candidateIsExtension ? 1 : 0;
        int currentParamIndex = currentIsExtension ? 1 : 0;

        if (candidateIsExtension && currentIsExtension && receiver is not null && receiver.Type is not null)
        {
            var candParamType = candParams[0].Type;
            var currentParamType = currentParams[0].Type;

            var candImplicit = IsImplicitConversion(compilation, receiver.Type, candParamType);
            var currentImplicit = IsImplicitConversion(compilation, receiver.Type, currentParamType);

            if (candImplicit && !currentImplicit)
            {
                better = true;
            }
            else if (!candImplicit && currentImplicit)
            {
                return false;
            }

            var candDist = GetInheritanceDistance(GetUnderlying(receiver.Type), GetUnderlying(candParamType));
            var currDist = GetInheritanceDistance(GetUnderlying(receiver.Type), GetUnderlying(currentParamType));

            if (candDist < currDist)
                better = true;
            else if (currDist < candDist)
                return false;
        }

        for (int i = 0; i < arguments.Length; i++)
        {
            var argType = arguments[i].Type;
            var candParamType = candParams[candParamIndex].Type;
            var currentParamType = currentParams[currentParamIndex].Type;

            if (argType is null)
            {
                candParamIndex++;
                currentParamIndex++;
                continue;
            }

            if (argType.TypeKind == TypeKind.Null)
            {
                var candImplicit = IsImplicitConversion(compilation, candParamType, currentParamType);
                var currentImplicit = IsImplicitConversion(compilation, currentParamType, candParamType);

                if (candImplicit && !currentImplicit)
                {
                    better = true;
                }
                else if (!candImplicit && currentImplicit)
                {
                    return false;
                }

                candParamIndex++;
                currentParamIndex++;
                continue;
            }

            var candType = GetUnderlying(candParamType);
            var currentType = GetUnderlying(currentParamType);
            var underlyingArgType = GetUnderlying(argType);

            var candDist = GetInheritanceDistance(underlyingArgType, candType);
            var currDist = GetInheritanceDistance(underlyingArgType, currentType);

            if (candDist < currDist)
                better = true;
            else if (currDist < candDist)
                return false;

            candParamIndex++;
            currentParamIndex++;
        }

        return better;
    }

    private static bool IsImplicitConversion(Compilation compilation, ITypeSymbol source, ITypeSymbol destination)
    {
        var conversion = compilation.ClassifyConversion(source, destination);
        return conversion.Exists && conversion.IsImplicit;
    }

    private static bool TryMatch(
        IMethodSymbol method,
        BoundExpression[] arguments,
        BoundExpression? receiver,
        bool treatAsExtension,
        Compilation compilation,
        out int score)
    {
        score = 0;
        int parameterIndex = 0;
        var parameters = method.Parameters;

        if (treatAsExtension)
        {
            if (receiver is null || receiver.Type is null)
                return false;

            if (!TryEvaluateArgument(parameters[parameterIndex], receiver, compilation, ref score))
                return false;

            parameterIndex++;
        }

        for (int i = 0; i < arguments.Length; i++, parameterIndex++)
        {
            if (!TryEvaluateArgument(parameters[parameterIndex], arguments[i], compilation, ref score))
                return false;
        }

        return true;
    }

    private static bool TryEvaluateArgument(IParameterSymbol parameter, BoundExpression argument, Compilation compilation, ref int score)
    {
        var argType = argument.Type;
        if (argType is null)
            return false;

        if (argType.SpecialType == SpecialType.System_Void)
            return false;

        if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
        {
            if (argument is not BoundAddressOfExpression addr ||
                addr.Type is not ByRefTypeSymbol argByRef ||
                !SymbolEqualityComparer.Default.Equals(argByRef.ElementType, parameter.Type) ||
                argType.SpecialType == SpecialType.System_Void)
            {
                return false;
            }

            return true;
        }

        if (argument is BoundAddressOfExpression)
            return false;

        var conversion = compilation.ClassifyConversion(argType, parameter.Type);
        if (!conversion.IsImplicit)
            return false;

        var conversionScore = GetConversionScore(conversion);

        if (parameter.Type is NullableTypeSymbol nullableParam && argType is not NullableTypeSymbol)
        {
            var liftedConversion = compilation.ClassifyConversion(argType, nullableParam.UnderlyingType);
            if (liftedConversion.Exists)
                conversionScore = GetConversionScore(liftedConversion);

            conversionScore++;
        }

        score += conversionScore;
        return true;
    }

    private static ITypeSymbol GetUnderlying(ITypeSymbol type) => type switch
    {
        NullableTypeSymbol nt => nt.UnderlyingType,
        LiteralTypeSymbol lt => lt.UnderlyingType,
        IUnionTypeSymbol ut => TypeSymbolExtensionsForCodeGen.FindCommonDenominator(ut.Types) ?? type,
        _ => type,
    };

    private static int GetInheritanceDistance(ITypeSymbol? derived, ITypeSymbol baseType)
    {
        int distance = 0;
        var current = derived;
        while (current is not null)
        {
            if (SymbolEqualityComparer.Default.Equals(current, baseType))
                return distance;
            current = current.BaseType;
            distance++;
        }
        return int.MaxValue;
    }

    private static int GetConversionScore(Conversion conversion)
    {
        if (!conversion.Exists)
            return int.MaxValue; // Not applicable, shouldn't occur during scoring

        if (conversion.IsIdentity)
            return 0;

        if (conversion.IsNumeric)
            return 1;

        if (conversion.IsReference)
            return 2;

        if (conversion.IsBoxing)
            return 3;

        if (conversion.IsUserDefined)
            return 4;

        if (conversion.IsUnboxing)
            return 5;

        return 10; // fallback or unspecified conversion
    }
}

internal readonly struct OverloadResolutionResult
{
    public OverloadResolutionResult(IMethodSymbol? method)
        : this(method, ImmutableArray<IMethodSymbol>.Empty)
    {
    }

    private OverloadResolutionResult(IMethodSymbol? method, ImmutableArray<IMethodSymbol> ambiguousCandidates)
    {
        Method = method;
        AmbiguousCandidates = ambiguousCandidates;
    }

    public IMethodSymbol? Method { get; }

    public ImmutableArray<IMethodSymbol> AmbiguousCandidates { get; }

    public bool Success => Method is not null && !IsAmbiguous;

    public bool IsAmbiguous => !AmbiguousCandidates.IsDefaultOrEmpty && AmbiguousCandidates.Length > 0;

    public static OverloadResolutionResult Ambiguous(ImmutableArray<IMethodSymbol> candidates)
    {
        if (candidates.IsDefault)
            candidates = ImmutableArray<IMethodSymbol>.Empty;

        return new OverloadResolutionResult(null, candidates);
    }
}
