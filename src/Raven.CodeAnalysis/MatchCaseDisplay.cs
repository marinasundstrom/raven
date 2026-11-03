using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class MatchCaseDisplay
{
    public static string ForDiscriminatedUnionCase(UnionCaseTypeSymbol caseSymbol)
    {
        var definition = caseSymbol.OriginalDefinition as UnionCaseTypeSymbol ?? caseSymbol;
        return definition.Name;
    }

    public static string ForDiscriminatedUnionCasePayload(
        UnionCaseTypeSymbol caseSymbol,
        TypeSymbol payloadType)
        => $"{ForDiscriminatedUnionCase(caseSymbol)}({ForPayload(payloadType)})";

    private static string ForPayload(TypeSymbol payloadType)
        => payloadType.TryGetUnionCase() is { } payloadCase
            ? $".{ForDiscriminatedUnionCase(payloadCase)}"
            : payloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
}
