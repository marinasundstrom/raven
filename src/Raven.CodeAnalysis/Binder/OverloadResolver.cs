namespace Raven.CodeAnalysis;

internal sealed class OverloadResolver
{
    public static IMethodSymbol? ResolveOverload(IEnumerable<IMethodSymbol> methods, BoundExpression[] arguments, Compilation compilation)
    {
        IMethodSymbol? bestMatch = null;
        int bestScore = int.MaxValue;

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

                // Handle ref/in/out matching
                if (param.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                {
                    if (arg is not BoundAddressOfExpression)
                    {
                        allMatch = false;
                        break;
                    }

                    // Must match the expected type exactly for ref/out
                    if (!SymbolEqualityComparer.Default.Equals(arg.Type, param.Type))
                    {
                        allMatch = false;
                        break;
                    }

                    // Favor exact match
                    continue;
                }
                else if (arg is BoundAddressOfExpression)
                {
                    // AddressOf used but parameter is not by-ref
                    allMatch = false;
                    break;
                }

                // Normal conversion
                var conversion = compilation.ClassifyConversion(arg.Type, param.Type);
                if (!conversion.IsImplicit)
                {
                    allMatch = false;
                    break;
                }

                // Implicit conversion score
                score += conversion.IsIdentity ? 0 : 1;
            }

            if (allMatch && score < bestScore)
            {
                bestMatch = method;
                bestScore = score;
            }
        }

        return bestMatch;
    }
}