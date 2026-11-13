using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class SequenceTypeUtilities
{
    public static TypeSymbol? TryGetElementType(SequenceTypeServices services, TypeSymbol type)
    {
        type = type.GetNonNullableType();
        if (type is LiteralTypeSymbol literal)
            type = literal.UnderlyingType;

        if (type is ArrayTypeSymbol arrayType)
            return arrayType.ElementType;

        if (type.SpecialType == SpecialType.System_String)
            return services.CharacterType;

        if (type is not NamedTypeSymbol namedType)
            return null;

        var synchronous = services.GetSequenceDefinition(asynchronous: false);
        var asynchronous = services.GetSequenceDefinition(asynchronous: true);
        foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
        {
            if (candidate.TypeArguments.Length == 1 &&
                (IsDefinition(candidate, synchronous) || IsDefinition(candidate, asynchronous)))
            {
                return candidate.TypeArguments[0];
            }
        }

        return null;
    }

    private static bool IsDefinition(NamedTypeSymbol candidate, NamedTypeSymbol? definition)
        => definition is not null && SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition, definition);

    private static IEnumerable<NamedTypeSymbol> EnumerateSelfAndInterfaces(NamedTypeSymbol type)
    {
        yield return type;
        foreach (var @interface in type.AllInterfaces)
            yield return @interface;
    }
}
