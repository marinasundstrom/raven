using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class DiscriminatedUnionFacts
{

    public static bool IsDiscriminatedUnionType(ITypeSymbol? type)
    {
        return type?.IsDiscriminatedUnion == true;
    }

    public static bool IsDiscriminatedUnionCaseType(ITypeSymbol? type)
    {
        return type?.IsDiscriminatedUnionCase == true;
    }

    public static bool TryGetCaseUnionType(ITypeSymbol? caseType, out ITypeSymbol? unionType)
    {
        unionType = caseType?.UnderlyingDiscriminatedUnion;
        return unionType is not null;
    }

    public static string GetCasePropertyName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName))
            return parameterName;

        if (char.IsUpper(parameterName[0]))
            return parameterName;

        Span<char> buffer = stackalloc char[parameterName.Length];
        parameterName.AsSpan().CopyTo(buffer);
        buffer[0] = char.ToUpperInvariant(buffer[0]);
        return new string(buffer);
    }

}
