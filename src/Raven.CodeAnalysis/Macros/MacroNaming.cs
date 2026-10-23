namespace Raven.CodeAnalysis.Macros;

internal static class MacroNaming
{
    public static string GetImplementationTypeName(string declaredName) => declaredName + "Macro";

    public static bool MatchesDeclaredName(string declaredName, string requestedName, bool allowSuffix)
        => string.Equals(declaredName, requestedName, StringComparison.Ordinal) ||
           (allowSuffix && string.Equals(GetImplementationTypeName(declaredName), requestedName, StringComparison.Ordinal));
}
