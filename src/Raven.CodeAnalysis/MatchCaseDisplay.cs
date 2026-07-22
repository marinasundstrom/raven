using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class MatchCaseDisplay
{
    public static string ForDiscriminatedUnionCase(IUnionCaseTypeSymbol caseSymbol)
    {
        var definition = caseSymbol.OriginalDefinition as IUnionCaseTypeSymbol ?? caseSymbol;
        return definition.Name;
    }

    public static string ForDiscriminatedUnionCasePayload(
        IUnionCaseTypeSymbol caseSymbol,
        ITypeSymbol payloadType)
        => $"{ForDiscriminatedUnionCase(caseSymbol)}({ForPayload(payloadType)})";

    private static string ForPayload(ITypeSymbol payloadType)
        => payloadType.TryGetUnionCase() is { } payloadCase
            ? $".{ForDiscriminatedUnionCase(payloadCase)}"
            : payloadType.ToDisplayStringKeywordAware(SymbolDisplayFormat.MinimallyQualifiedFormat);
}
