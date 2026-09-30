using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis;

internal static class ParameterConversionQueries
{
    internal static bool TryScore(ITypeSymbol? argumentType, IMethodSymbol method, int parameterIndex, ref int score, out bool handled)
    {
        handled = false;
        if (argumentType is null || argumentType.TypeKind == TypeKind.Error ||
            method is not IParameterConversionClassifier classifier ||
            !classifier.TryClassifyParameterConversion(argumentType, parameterIndex, out var conversion))
        {
            return false;
        }

        handled = true;
        switch (conversion)
        {
            case ParameterConversionKind.Identity:
                score += 8;
                return true;
            case ParameterConversionKind.ImplicitNumeric:
                score += 5;
                return true;
            case ParameterConversionKind.ToObject:
                score += 4;
                return true;
            default:
                return false;
        }
    }
}
