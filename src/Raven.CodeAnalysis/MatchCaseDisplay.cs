namespace Raven.CodeAnalysis;

internal static class MatchCaseDisplay
{
    public static string ForDiscriminatedUnionCase(IDiscriminatedUnionCaseSymbol caseSymbol)
    {
        var definition = caseSymbol.OriginalDefinition as IDiscriminatedUnionCaseSymbol ?? caseSymbol;
        return definition.Name;
    }
}
