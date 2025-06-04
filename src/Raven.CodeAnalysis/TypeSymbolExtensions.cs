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

        // Handle arrays
        if (typeSymbol is IArrayTypeSymbol arrayTypeSymbol)
        {
            var elementClrType = arrayTypeSymbol.ElementType.GetClrType(compilation);
            return elementClrType?.MakeArrayType(); //arrayTypeSymbol.Rank);
        }

        // Handle special types (e.g., int, string, etc.)
        if (typeSymbol.SpecialType is not SpecialType.None)
        {
            return GetFrameworkType(typeSymbol.SpecialType, compilation);
        }

        // Handle named types (classes, structs, enums, etc.)
        if (typeSymbol is INamedTypeSymbol namedTypeSymbol)
        {
            // Attempt to resolve the fully qualified name
            var fullyQualifiedName = namedTypeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            return compilation.CoreAssembly.GetType(fullyQualifiedName, throwOnError: false)!;
        }

        // Handle dynamic type
        if (typeSymbol is IUnionTypeSymbol)
        {
            return GetFrameworkType(SpecialType.System_Object, compilation);
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

    private static Type GetFrameworkType(SpecialType specialType, Compilation compilation)
    {
        // Helper to fetch type from the MetadataLoadContext CoreAssembly
        static Type FromCoreAssembly(Compilation c, string fullName) =>
            c.CoreAssembly.GetType(fullName) ?? throw new InvalidOperationException($"Type '{fullName}' not found in CoreAssembly");

        return specialType switch
        {
            SpecialType.System_Int32 => FromCoreAssembly(compilation, "System.Int32"),
            SpecialType.System_String => FromCoreAssembly(compilation, "System.String"),
            SpecialType.System_Boolean => FromCoreAssembly(compilation, "System.Boolean"),
            SpecialType.System_Object => FromCoreAssembly(compilation, "System.Object"),
            SpecialType.System_Void => FromCoreAssembly(compilation, "System.Void"),
            SpecialType.System_Double => FromCoreAssembly(compilation, "System.Double"),
            SpecialType.System_Char => FromCoreAssembly(compilation, "System.Char"),
            SpecialType.System_Int64 => FromCoreAssembly(compilation, "System.Int64"),
            SpecialType.System_Single => FromCoreAssembly(compilation, "System.Single"),
            SpecialType.System_Byte => FromCoreAssembly(compilation, "System.Byte"),
            SpecialType.System_Decimal => FromCoreAssembly(compilation, "System.Decimal"),
            SpecialType.System_Int16 => FromCoreAssembly(compilation, "System.Int16"),
            SpecialType.System_UInt32 => FromCoreAssembly(compilation, "System.UInt32"),
            SpecialType.System_UInt64 => FromCoreAssembly(compilation, "System.UInt64"),
            SpecialType.System_UInt16 => FromCoreAssembly(compilation, "System.UInt16"),
            SpecialType.System_SByte => FromCoreAssembly(compilation, "System.SByte"),
            SpecialType.System_DateTime => FromCoreAssembly(compilation, "System.DateTime"),
            SpecialType.System_Array => FromCoreAssembly(compilation, "System.Array"),
            SpecialType.System_Type => FromCoreAssembly(compilation, "System.Type"),
            _ => throw new NotSupportedException($"Unsupported special type: {specialType}")
        };
    }
}