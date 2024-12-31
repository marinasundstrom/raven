namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensions
{
    /// <summary>
    /// Resolves the CLR Type from a Roslyn ITypeSymbol.
    /// </summary>
    /// <param name="typeSymbol">The ITypeSymbol to resolve.</param>
    /// <param name="compilation">The compilation context used for resolving symbols.</param>
    /// <returns>The corresponding CLR Type, or null if the type cannot be resolved.</returns>
    public static Type GetClrType(this ITypeSymbol typeSymbol, Compilation compilation)
    {
        if (typeSymbol == null)
            throw new ArgumentNullException(nameof(typeSymbol));
        if (compilation == null)
            throw new ArgumentNullException(nameof(compilation));

        // Handle special types (e.g., int, string, etc.)
        if (typeSymbol.SpecialType != SpecialType.None)
        {
            return GetFrameworkType(typeSymbol.SpecialType);
        }

        /*
        // Handle arrays
        if (typeSymbol is IArrayTypeSymbol arrayTypeSymbol)
        {
            var elementClrType = arrayTypeSymbol.ElementType.GetClrType(compilation);
            return elementClrType?.MakeArrayType(arrayTypeSymbol.Rank);
        }
        */

        // Handle named types (classes, structs, enums, etc.)
        if (typeSymbol is INamedTypeSymbol namedTypeSymbol)
        {
            // Attempt to resolve the fully qualified name
            var fullyQualifiedName = namedTypeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            return Type.GetType(fullyQualifiedName, throwOnError: false)!;
        }

        /*
        // Handle dynamic type
        if (typeSymbol is IDynamicTypeSymbol)
        {
            return typeof(object); // 'dynamic' maps to 'System.Object'
        }
        */

        /*
        // Handle pointer types
        if (typeSymbol is IPointerTypeSymbol pointerTypeSymbol)
        {
            var elementClrType = pointerTypeSymbol.PointedAtType.GetClrType(compilation);
            return elementClrType?.MakePointerType();
        }
        */

        // Unsupported cases
        throw new NotSupportedException($"Unsupported type symbol: {typeSymbol}");
    }

    private static Type GetFrameworkType(SpecialType specialType)
    {
        // Map Roslyn SpecialType to CLR types
        return specialType switch
        {
            SpecialType.System_Int32 => typeof(int),
            SpecialType.System_String => typeof(string),
            SpecialType.System_Boolean => typeof(bool),
            SpecialType.System_Object => typeof(object),
            SpecialType.System_Void => typeof(void),
            SpecialType.System_Double => typeof(double),
            SpecialType.System_Char => typeof(char),
            SpecialType.System_Int64 => typeof(long),
            SpecialType.System_Single => typeof(float),
            SpecialType.System_Byte => typeof(byte),
            SpecialType.System_Decimal => typeof(decimal),
            SpecialType.System_Int16 => typeof(short),
            SpecialType.System_UInt32 => typeof(uint),
            SpecialType.System_UInt64 => typeof(ulong),
            SpecialType.System_UInt16 => typeof(ushort),
            SpecialType.System_SByte => typeof(sbyte),
            SpecialType.System_DateTime => typeof(DateTime),
            _ => null!
        };
    }
}