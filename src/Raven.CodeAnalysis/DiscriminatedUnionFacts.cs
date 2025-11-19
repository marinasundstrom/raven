using System;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class DiscriminatedUnionFacts
{
    private const string DiscriminatedUnionAttributeName = "System.Runtime.CompilerServices.DiscriminatedUnionAttribute";
    private const string DiscriminatedUnionCaseAttributeName = "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute";

    public static bool IsDiscriminatedUnionType(ITypeSymbol? type)
    {
        if (type is null)
            return false;

        if (type.TryGetDiscriminatedUnion() is not null)
            return true;

        return HasAttribute(type, DiscriminatedUnionAttributeName);
    }

    public static bool IsDiscriminatedUnionCaseType(ITypeSymbol? type)
    {
        if (type is null)
            return false;

        if (type.TryGetDiscriminatedUnionCase() is not null)
            return true;

        return HasAttribute(type, DiscriminatedUnionCaseAttributeName);
    }

    public static bool TryGetCaseUnionType(ITypeSymbol? caseType, out ITypeSymbol? unionType)
    {
        if (caseType?.TryGetDiscriminatedUnionCase() is { } caseSymbol)
        {
            unionType = caseSymbol.Union;
            return true;
        }

        if (caseType is not null)
        {
            foreach (var attribute in caseType.GetAttributes())
            {
                if (IsAttribute(attribute, DiscriminatedUnionCaseAttributeName) &&
                    attribute.ConstructorArguments.Length == 1)
                {
                    var argument = attribute.ConstructorArguments[0];
                    if (argument.Kind == TypedConstantKind.Type && argument.Value is ITypeSymbol union)
                    {
                        unionType = union;
                        return true;
                    }
                }
            }
        }

        unionType = null;
        return false;
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

    private static bool HasAttribute(ITypeSymbol type, string metadataName)
        => type.GetAttributes().Any(attribute => IsAttribute(attribute, metadataName));

    private static bool IsAttribute(AttributeData attribute, string metadataName)
        => attribute.AttributeClass?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) == metadataName;
}
