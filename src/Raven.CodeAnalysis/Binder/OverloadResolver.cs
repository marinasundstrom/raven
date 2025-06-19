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

                // Handle ref/in/out
                if (param.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                {
                    if (arg is not BoundAddressOfExpression ||
                        !SymbolEqualityComparer.Default.Equals(arg.Type, param.Type))
                    {
                        allMatch = false;
                        break;
                    }

                    continue; // no score increase for ref match
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
                    ambiguous = true;
                }
            }
        }

        return ambiguous ? null : bestMatch;
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