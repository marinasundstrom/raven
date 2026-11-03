using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class SequenceTypeUtilities
{
    public static TypeSymbol? TryGetElementType(Compilation compilation, TypeSymbol type)
    {
        type = type.GetNonNullableType();
        type = type.UnwrapLiteralType() ?? type;

        if (type is ArrayTypeSymbol arrayType)
            return arrayType.ElementType;

        if (type.SpecialType == SpecialType.System_String)
            return compilation.GetSpecialType(SpecialType.System_Char);

        if (type is not NamedTypeSymbol namedType)
            return null;

        foreach (var candidate in EnumerateSelfAndInterfaces(namedType))
        {
            if (candidate.TypeArguments.Length == 1 &&
                candidate.Name is "IEnumerable" or "IAsyncEnumerable")
            {
                return candidate.TypeArguments[0];
            }
        }

        return null;
    }

    private static IEnumerable<NamedTypeSymbol> EnumerateSelfAndInterfaces(NamedTypeSymbol type)
    {
        yield return type;
        foreach (var @interface in type.AllInterfaces)
            yield return @interface;
    }
}
