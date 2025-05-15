namespace Raven.CodeAnalysis;

internal static class SymbolDisplayFormatExtensions
{
    public static SymbolDisplayFormat UseSpecialTypesIfEnabled(this SymbolDisplayFormat format)
    {
        if (format.MiscellaneousOptions.HasFlag(SymbolDisplayMiscellaneousOptions.UseSpecialTypes))
            return format; // already enables keyword-style types

        // optionally wrap or clone if keyword rendering depends on a different path
        return format;
    }
}