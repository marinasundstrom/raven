using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis;

internal static class MethodParameterQueries
{
    internal static bool TryGetCount(IMethodSymbol method, out int count)
    {
        if (method is IMethodParameterInfo info)
            return info.TryGetParameterCount(out count);

        count = method.Parameters.Length;
        return true;
    }

    internal static bool TryGetType(IMethodSymbol method, int index, [NotNullWhen(true)] out ITypeSymbol? type)
    {
        if (method is IMethodParameterInfo info)
            return info.TryGetParameterType(index, out type);

        var parameters = method.Parameters;
        if (index < 0 || index >= parameters.Length)
        {
            type = null;
            return false;
        }

        type = parameters[index].Type;
        return type is not null;
    }

    internal static bool TryGetRequiredCount(IMethodSymbol method, int parameterOffset, out int requiredCount, out bool hasParams)
    {
        requiredCount = 0;
        hasParams = false;
        if (!TryGetCount(method, out var parameterCount) || parameterOffset < 0 || parameterOffset > parameterCount)
            return false;

        if (method is IMethodParameterInfo info)
        {
            for (var i = parameterOffset; i < parameterCount; i++)
            {
                if (!info.TryGetParameterUsage(i, out var isOptional, out var isVariadic))
                    return false;

                hasParams |= isVariadic;
                if (!isOptional && !isVariadic)
                    requiredCount++;
            }
            return true;
        }

        var parameters = method.Parameters;
        for (var i = parameterOffset; i < parameters.Length; i++)
        {
            var parameter = parameters[i];
            hasParams |= parameter.IsVarParams;
            if (!parameter.HasExplicitDefaultValue && !parameter.IsVarParams)
                requiredCount++;
        }
        return true;
    }
}
