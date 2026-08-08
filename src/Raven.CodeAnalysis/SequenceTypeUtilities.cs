using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class SequenceTypeUtilities
{
    public static ITypeSymbol? TryGetElementType(Compilation compilation, ITypeSymbol type)
    {
        type = type.GetNonNullableType();
        type = type.UnwrapLiteralType() ?? type;

        if (type is IArrayTypeSymbol arrayType)
            return arrayType.ElementType;

        if (type.SpecialType == SpecialType.System_String)
            return compilation.GetSpecialType(SpecialType.System_Char);

        if (type is not INamedTypeSymbol namedType)
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

    private static IEnumerable<INamedTypeSymbol> EnumerateSelfAndInterfaces(INamedTypeSymbol type)
    {
        yield return type;
        foreach (var @interface in type.AllInterfaces)
            yield return @interface;
    }
}
