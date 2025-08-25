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

                // No value can have type 'void' — immediately reject this candidate.
                if (arg.Type is { } t && t.SpecialType == SpecialType.System_Void)
                {
                    allMatch = false;
                    break;
                }

                // Handle ref/in/out
                if (param.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                {
                    if (arg is not BoundAddressOfExpression ||
                        !SymbolEqualityComparer.Default.Equals(arg.Type, param.Type) ||
                        arg.Type.SpecialType == SpecialType.System_Void) // extra guard for safety
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

                var conversion = compilation.ClassifyConversion(arg.Type, param.Type);
                if (!conversion.IsImplicit)
                {
                    allMatch = false;
                    break;
                }

                score += GetConversionScore(conversion);

                if (param.Type is NullableTypeSymbol && arg.Type is not NullableTypeSymbol)
                    score++;
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
                    if (IsMoreSpecific(method, bestMatch!, arguments))
                    {
                        bestMatch = method;
                        ambiguous = false;
                    }
                    else if (!IsMoreSpecific(bestMatch!, method, arguments))
                    {
                        ambiguous = true;
                    }
                }
            }
        }

        return ambiguous ? null : bestMatch;
    }

    private static bool IsMoreSpecific(IMethodSymbol candidate, IMethodSymbol current, BoundExpression[] arguments)
    {
        bool better = false;
        var candParams = candidate.Parameters;
        var currentParams = current.Parameters;

        for (int i = 0; i < arguments.Length; i++)
        {
            var argType = GetUnderlying(arguments[i].Type);
            var candType = GetUnderlying(candParams[i].Type);
            var currentType = GetUnderlying(currentParams[i].Type);

            var candDist = GetInheritanceDistance(argType, candType);
            var currDist = GetInheritanceDistance(argType, currentType);

            if (candDist < currDist)
                better = true;
            else if (currDist < candDist)
                return false;
        }

        return better;
    }

    private static ITypeSymbol GetUnderlying(ITypeSymbol type) =>
        type is NullableTypeSymbol nt ? nt.UnderlyingType : type;

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
