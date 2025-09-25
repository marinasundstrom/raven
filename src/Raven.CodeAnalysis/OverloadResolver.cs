using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class OverloadResolver
{
    public static OverloadResolutionResult ResolveOverload(
        IEnumerable<IMethodSymbol> methods,
        BoundExpression[] arguments,
        Compilation compilation)
    {
        IMethodSymbol? bestMatch = null;
        int bestScore = int.MaxValue;
        ImmutableArray<IMethodSymbol>.Builder? ambiguous = null;

        foreach (var originalMethod in methods)
        {
            if (originalMethod.Parameters.Length != arguments.Length)
                continue;

            var method = PrepareCandidate(originalMethod, arguments);
            if (method is null)
                continue;

            var parameters = method.Parameters;
            int score = 0;
            bool allMatch = true;

            for (int i = 0; i < arguments.Length; i++)
            {
                var param = parameters[i];
                var arg = arguments[i];

                var argType = arg.Type;
                if (argType is null)
                {
                    allMatch = false;
                    break;
                }

                // No value can have type 'void' — immediately reject this candidate.
                if (argType.SpecialType == SpecialType.System_Void)
                {
                    allMatch = false;
                    break;
                }

                // Handle ref/in/out
                if (param.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                {
                    if (arg is not BoundAddressOfExpression addr ||
                        addr.Type is not ByRefTypeSymbol argByRef ||
                        !SymbolEqualityComparer.Default.Equals(argByRef.ElementType, param.Type) ||
                        argType.SpecialType == SpecialType.System_Void)
                    {
                        allMatch = false;
                        break;
                    }

                    continue; // exact ref match; no score increase
                }

                if (arg is BoundAddressOfExpression)
                {
                    // AddressOf passed but parameter not expecting it
                    allMatch = false;
                    break;
                }

                var conversion = compilation.ClassifyConversion(argType, param.Type);
                if (!conversion.IsImplicit)
                {
                    allMatch = false;
                    break;
                }

                var conversionScore = GetConversionScore(conversion);

                if (param.Type is NullableTypeSymbol nullableParam && argType is not NullableTypeSymbol)
                {
                    var liftedConversion = compilation.ClassifyConversion(argType, nullableParam.UnderlyingType);
                    if (liftedConversion.Exists)
                        conversionScore = GetConversionScore(liftedConversion);

                    conversionScore++;
                }

                score += conversionScore;
            }

            if (!allMatch)
                continue;

            if (score < bestScore)
            {
                bestMatch = method;
                bestScore = score;
                ambiguous = null;
                continue;
            }

            if (score > bestScore || bestMatch is null)
                continue;

            if (IsMoreSpecific(method, bestMatch, arguments, compilation))
            {
                bestMatch = method;
                ambiguous = null;
                continue;
            }

            if (IsMoreSpecific(bestMatch, method, arguments, compilation))
                continue;

            ambiguous ??= ImmutableArray.CreateBuilder<IMethodSymbol>();
            AddCandidateIfMissing(ambiguous, bestMatch);
            AddCandidateIfMissing(ambiguous, method);
        }

        if (ambiguous is { Count: > 0 })
            return OverloadResolutionResult.Ambiguous(ambiguous.ToImmutable());

        return new OverloadResolutionResult(bestMatch);
    }

    private static IMethodSymbol? PrepareCandidate(
        IMethodSymbol method,
        BoundExpression[] arguments)
    {
        if (method.TypeParameters.IsDefaultOrEmpty || method.TypeParameters.Length == 0)
            return method;

        if (!HasOpenTypeArguments(method))
            return method;

        return TryInferMethodTypeArguments(method, arguments);
    }

    private static bool HasOpenTypeArguments(IMethodSymbol method)
    {
        if (method.TypeArguments.IsDefaultOrEmpty || method.TypeArguments.Length == 0)
            return false;

        foreach (var typeArgument in method.TypeArguments)
        {
            if (typeArgument is ITypeParameterSymbol tp && ContainsMethodTypeParameter(method.TypeParameters, tp))
                return true;
        }

        return false;
    }

    private static IMethodSymbol? TryInferMethodTypeArguments(
        IMethodSymbol method,
        BoundExpression[] arguments)
    {
        var typeParameters = method.TypeParameters;
        if (typeParameters.IsDefaultOrEmpty || typeParameters.Length == 0)
            return method;

        var map = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(SymbolEqualityComparer.Default);

        var parameters = method.Parameters;

        for (int i = 0; i < parameters.Length; i++)
        {
            var argumentType = GetArgumentTypeForInference(arguments[i]);
            if (argumentType is null || argumentType.TypeKind == TypeKind.Error)
                return null;

            if (!TryUnifyParameter(
                    parameters[i].Type,
                    argumentType,
                    typeParameters,
                    map))
            {
                return null;
            }
        }

        if (map.Count == 0)
            return null;

        var inferredArguments = new ITypeSymbol[typeParameters.Length];

        for (int i = 0; i < typeParameters.Length; i++)
        {
            var typeParameter = typeParameters[i];
            if (!map.TryGetValue(typeParameter, out var inferred))
                return null;

            inferredArguments[i] = inferred;
        }

        try
        {
            return method.Construct(inferredArguments);
        }
        catch
        {
            return null;
        }
    }

    private static ITypeSymbol? GetArgumentTypeForInference(BoundExpression argument)
    {
        if (argument is BoundAddressOfExpression addressOf)
            return addressOf.ValueType;

        return argument.Type;
    }

    private static bool TryUnifyParameter(
        ITypeSymbol parameterType,
        ITypeSymbol argumentType,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters,
        Dictionary<ITypeParameterSymbol, ITypeSymbol> map)
    {
        parameterType = Normalize(parameterType);
        argumentType = Normalize(argumentType);

        if (parameterType is ITypeParameterSymbol paramTypeParameter)
        {
            if (!ContainsMethodTypeParameter(methodTypeParameters, paramTypeParameter))
                return true;

            if (argumentType.TypeKind == TypeKind.Null)
                return false;

            if (map.TryGetValue(paramTypeParameter, out var existing))
                return SymbolEqualityComparer.Default.Equals(existing, argumentType);

            map[paramTypeParameter] = argumentType;
            return true;
        }

        if (!ContainsMethodTypeParameter(parameterType, methodTypeParameters))
            return true;

        switch (parameterType)
        {
            case NullableTypeSymbol nullableParameter:
                {
                    if (argumentType is NullableTypeSymbol nullableArgument)
                        return TryUnifyParameter(nullableParameter.UnderlyingType, nullableArgument.UnderlyingType, methodTypeParameters, map);

                    if (argumentType.TypeKind == TypeKind.Null)
                        return false;

                    return TryUnifyParameter(nullableParameter.UnderlyingType, argumentType, methodTypeParameters, map);
                }

            case IArrayTypeSymbol arrayParameter when argumentType is IArrayTypeSymbol arrayArgument:
                return TryUnifyParameter(arrayParameter.ElementType, arrayArgument.ElementType, methodTypeParameters, map);

            case INamedTypeSymbol namedParameter:
                {
                    if (argumentType is not INamedTypeSymbol namedArgument)
                        return false;

                    var match = FindMatchingConstructedType(namedArgument, namedParameter);
                    if (match is null)
                        return false;

                    var parameterArguments = namedParameter.TypeArguments;
                    var argumentArguments = match.TypeArguments;

                    if (parameterArguments.Length != argumentArguments.Length)
                        return false;

                    for (int i = 0; i < parameterArguments.Length; i++)
                    {
                        if (!TryUnifyParameter(parameterArguments[i], argumentArguments[i], methodTypeParameters, map))
                            return false;
                    }

                    return true;
                }

            case IUnionTypeSymbol unionParameter:
                {
                    foreach (var branch in unionParameter.Types)
                    {
                        var snapshot = new Dictionary<ITypeParameterSymbol, ITypeSymbol>(map, SymbolEqualityComparer.Default);
                        if (TryUnifyParameter(branch, argumentType, methodTypeParameters, snapshot))
                        {
                            foreach (var entry in snapshot)
                                map[entry.Key] = entry.Value;

                            return true;
                        }
                    }

                    return false;
                }
        }

        return false;
    }

    private static bool ContainsMethodTypeParameter(
        ITypeSymbol type,
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters)
    {
        type = Normalize(type);

        return type switch
        {
            ITypeParameterSymbol tp => ContainsMethodTypeParameter(methodTypeParameters, tp),
            INamedTypeSymbol named => named.TypeArguments.Any(arg => ContainsMethodTypeParameter(arg, methodTypeParameters)),
            IArrayTypeSymbol array => ContainsMethodTypeParameter(array.ElementType, methodTypeParameters),
            NullableTypeSymbol nullable => ContainsMethodTypeParameter(nullable.UnderlyingType, methodTypeParameters),
            IUnionTypeSymbol union => union.Types.Any(arg => ContainsMethodTypeParameter(arg, methodTypeParameters)),
            _ => false,
        };
    }

    private static bool ContainsMethodTypeParameter(
        ImmutableArray<ITypeParameterSymbol> methodTypeParameters,
        ITypeParameterSymbol candidate)
    {
        foreach (var methodTypeParameter in methodTypeParameters)
        {
            if (SymbolEqualityComparer.Default.Equals(methodTypeParameter, candidate))
                return true;
        }

        return false;
    }

    private static INamedTypeSymbol? FindMatchingConstructedType(
        INamedTypeSymbol argumentType,
        INamedTypeSymbol parameterType)
    {
        var targetDefinition = GetGenericDefinition(parameterType);

        for (INamedTypeSymbol? current = argumentType; current is not null; current = current.BaseType)
        {
            if (AreSameGenericDefinition(current, targetDefinition))
                return current;
        }

        foreach (var interfaceType in argumentType.AllInterfaces)
        {
            if (AreSameGenericDefinition(interfaceType, targetDefinition))
                return interfaceType;
        }

        return null;
    }

    private static bool AreSameGenericDefinition(INamedTypeSymbol candidate, INamedTypeSymbol targetDefinition)
    {
        var candidateDefinition = GetGenericDefinition(candidate);
        return SymbolEqualityComparer.Default.Equals(candidateDefinition, targetDefinition);
    }

    private static INamedTypeSymbol GetGenericDefinition(INamedTypeSymbol symbol)
    {
        var constructedFrom = symbol.ConstructedFrom as INamedTypeSymbol;
        if (constructedFrom is not null && !SymbolEqualityComparer.Default.Equals(constructedFrom, symbol))
            return GetGenericDefinition(constructedFrom);

        if (symbol.OriginalDefinition is INamedTypeSymbol original && !SymbolEqualityComparer.Default.Equals(original, symbol))
            return GetGenericDefinition(original);

        return symbol;
    }

    private static ITypeSymbol Normalize(ITypeSymbol type)
    {
        while (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            type = alias;

        switch (type)
        {
            case LiteralTypeSymbol literal:
                return Normalize(literal.UnderlyingType);
            case ByRefTypeSymbol byRef:
                return Normalize(byRef.ElementType);
            default:
                return type;
        }
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
        Compilation compilation)
    {
        bool better = false;
        var candParams = candidate.Parameters;
        var currentParams = current.Parameters;

        for (int i = 0; i < arguments.Length; i++)
        {
            var argType = arguments[i].Type;
            var candParamType = candParams[i].Type;
            var currentParamType = currentParams[i].Type;

            if (argType is null)
                continue;

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
        }

        return better;
    }

    private static bool IsImplicitConversion(Compilation compilation, ITypeSymbol source, ITypeSymbol destination)
    {
        var conversion = compilation.ClassifyConversion(source, destination);
        return conversion.Exists && conversion.IsImplicit;
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
