namespace Raven.CodeAnalysis;

internal static class MatchCaseDisplay
{
    public static string ForDiscriminatedUnionCase(IUnionCaseTypeSymbol caseSymbol)
    {
        var definition = caseSymbol.OriginalDefinition as IUnionCaseTypeSymbol ?? caseSymbol;
        return definition.Name;
    }
}
