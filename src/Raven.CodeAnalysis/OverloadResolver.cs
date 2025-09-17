using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class OverloadResolver
{
    public static IMethodSymbol? ResolveOverload(
        IEnumerable<IMethodSymbol> methods,
        BoundExpression[] arguments,
        Compilation compilation)
    {
        IMethodSymbol? bestMatch = null;
        int bestScore = int.MaxValue;
        bool ambiguous = false;

        foreach (var method in methods)
        {
            var parameters = method.Parameters;
            if (parameters.Length != arguments.Length)
                continue;

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

            if (allMatch)
            {
                if (score < bestScore)
                {
                    bestMatch = method;
                    bestScore = score;
                    ambiguous = false;
                }
                else if (score == bestScore)
                {
                    if (IsMoreSpecific(method, bestMatch!, arguments, compilation))
                    {
                        bestMatch = method;
                        ambiguous = false;
                    }
                    else if (!IsMoreSpecific(bestMatch!, method, arguments, compilation))
                    {
                        ambiguous = true;
                    }
                }
            }
        }

        return ambiguous ? null : bestMatch;
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
