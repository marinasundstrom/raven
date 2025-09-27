using System.Collections.Generic;
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

        foreach (var candidate in methods)
        {
            var method = ApplyTypeArgumentInference(candidate, receiver, arguments, compilation);
            if (method is null)
                continue;

            var parameters = method.Parameters;
            var treatAsExtension = method.IsExtensionMethod && receiver is not null;
            var providedCount = arguments.Length + (treatAsExtension ? 1 : 0);

            if (!HasSufficientArguments(parameters, providedCount))
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

    internal static IMethodSymbol? ApplyTypeArgumentInference(
        IMethodSymbol method,
        BoundExpression? receiver,
        BoundExpression[] arguments,
        Compilation compilation)
    {
        if (!method.IsGenericMethod || method.TypeParameters.IsDefaultOrEmpty || method.TypeParameters.Length == 0)
            return method;

        if (!ReferenceEquals(method, method.ConstructedFrom))
            return method;

        var treatAsExtension = method.IsExtensionMethod && receiver is not null;
        return TryConstructMethodWithInference(method, receiver, arguments, treatAsExtension, compilation);
    }

    private static IMethodSymbol? TryConstructMethodWithInference(
        IMethodSymbol method,
        BoundExpression? receiver,
        BoundExpression[] arguments,
        bool treatAsExtension,
        Compilation compilation)
    {
        var substitutions = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);
        var parameters = method.Parameters;
        var parameterIndex = 0;

        if (treatAsExtension)
        {
            if (receiver?.Type is null)
                return null;

            if (!TryInferFromTypes(compilation, parameters[parameterIndex].Type, receiver.Type, substitutions))
                return null;

            parameterIndex++;
        }

        for (int i = 0; i < arguments.Length && parameterIndex < parameters.Length; i++, parameterIndex++)
        {
            var argumentType = arguments[i].Type;
            if (argumentType is null)
                continue;

            if (!TryInferFromTypes(compilation, parameters[parameterIndex].Type, argumentType, substitutions))
                return null;
        }

        var inferredArguments = new ITypeSymbol[method.TypeParameters.Length];
        for (int i = 0; i < method.TypeParameters.Length; i++)
        {
            var typeParameter = method.TypeParameters[i];
            if (!substitutions.TryGetValue(typeParameter, out var inferred))
                return null;

            inferredArguments[i] = NormalizeType(inferred);
        }

        return method.Construct(inferredArguments);
    }

    private static bool TryInferFromTypes(
        Compilation compilation,
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> substitutions)
    {
        parameterType = NormalizeType(parameterType);
        argumentType = NormalizeType(argumentType);

        if (argumentType.TypeKind == TypeKind.Error)
            return false;

        if (parameterType is ITypeParameterSymbol typeParameter)
        {
            argumentType = NormalizeType(argumentType);

            if (substitutions.TryGetValue(typeParameter, out var existing))
            {
                existing = NormalizeType(existing);

                if (SymbolEqualityComparer.Default.Equals(existing, argumentType))
                    return true;

                if (compilation.ClassifyConversion(argumentType, existing).IsImplicit)
                    return true;

                if (compilation.ClassifyConversion(existing, argumentType).IsImplicit)
                {
                    substitutions[typeParameter] = argumentType;
                    return true;
                }

                return false;
            }

            substitutions[typeParameter] = argumentType;
            return true;
        }

        if (parameterType is INamedTypeSymbol paramNamed && argumentType is INamedTypeSymbol argNamed)
        {
            if (SymbolEqualityComparer.Default.Equals(paramNamed.OriginalDefinition, argNamed.OriginalDefinition))
            {
                var paramArguments = paramNamed.TypeArguments;
                var argArguments = argNamed.TypeArguments;

                for (int i = 0; i < paramArguments.Length; i++)
                {
                    if (!TryInferFromTypes(compilation, paramArguments[i], argArguments[i], substitutions))
                        return false;
                }

                return true;
            }
        }

        if (parameterType is IArrayTypeSymbol paramArray && argumentType is IArrayTypeSymbol argArray)
            return TryInferFromTypes(compilation, paramArray.ElementType, argArray.ElementType, substitutions);

        if (parameterType is NullableTypeSymbol paramNullable && argumentType is NullableTypeSymbol argNullable)
            return TryInferFromTypes(compilation, paramNullable.UnderlyingType, argNullable.UnderlyingType, substitutions);

        return true;
    }

    private static ITypeSymbol NormalizeType(ITypeSymbol type)
    {
        if (type is LiteralTypeSymbol literal)
            return literal.UnderlyingType;

        return type;
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

        for (; parameterIndex < parameters.Length; parameterIndex++)
        {
            if (!parameters[parameterIndex].HasExplicitDefaultValue)
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
                argType.SpecialType == SpecialType.System_Void)
            {
                return false;
            }

            var parameterType = parameter.Type;

            if (parameterType is ByRefTypeSymbol paramByRef)
            {
                if (!SymbolEqualityComparer.Default.Equals(argByRef.ElementType, paramByRef.ElementType))
                    return false;
            }
            else if (!SymbolEqualityComparer.Default.Equals(argByRef.ElementType, parameterType))
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

    private static bool HasSufficientArguments(ImmutableArray<IParameterSymbol> parameters, int providedCount)
    {
        if (providedCount > parameters.Length)
            return false;

        var required = GetRequiredParameterCount(parameters);
        return providedCount >= required;
    }

    private static int GetRequiredParameterCount(ImmutableArray<IParameterSymbol> parameters)
    {
        var required = parameters.Length;
        while (required > 0 && parameters[required - 1].HasExplicitDefaultValue)
            required--;

        return required;
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
