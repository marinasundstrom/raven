using Raven.CodeAnalysis.Symbols;

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

        // Handle constructed type
        if (typeSymbol is ConstructedNamedTypeSymbol constructedNamedType)
        {
            var baseType = constructedNamedType.ConstructedFrom.GetClrType(compilation);

            List<Type> typeArguments = new List<Type>();

            foreach (var a in constructedNamedType.TypeArguments)
            {
                typeArguments.Add(a.GetClrType(compilation));
            }

            return baseType.MakeGenericType(typeArguments.ToArray());
        }

        // Handle named types (classes, structs, enums, etc.)
        if (typeSymbol is INamedTypeSymbol namedTypeSymbol)
        {
            var metadataName = namedTypeSymbol.ToFullyQualifiedMetadataName();

            var runtimeType = Type.GetType(metadataName, throwOnError: false);
            if (runtimeType is not null)
                return runtimeType;

            var metadataType = compilation.CoreAssembly.GetType(metadataName, throwOnError: false);

            if (metadataType is not null)
                return metadataType;

            throw new InvalidOperationException($"Unable to resolve metadata type '{metadataName}' from the core assembly.");
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

        // Handle pointer types
        if (typeSymbol is IPointerTypeSymbol pointerTypeSymbol)
        {
            var elementClrType = pointerTypeSymbol.PointedAtType.GetClrType(compilation);
            if (elementClrType is null)
                throw new NotSupportedException($"Unsupported pointer element type: {pointerTypeSymbol.PointedAtType}");

            return elementClrType.MakePointerType();
        }

        // Unsupported cases
        throw new NotSupportedException($"Unsupported type symbol: {typeSymbol}");
    }

    private static Type GetFrameworkType(SpecialType specialType, Compilation compilation)
    {
        // Helper to fetch type from the MetadataLoadContext CoreAssembly
        static Type Resolve(Compilation c, string fullName)
        {
            var runtimeType = c.ResolveRuntimeType(fullName);
            if (runtimeType is not null)
                return runtimeType;

            return c.CoreAssembly.GetType(fullName)
                ?? throw new InvalidOperationException($"Type '{fullName}' not found in referenced assemblies.");
        }

        return specialType switch
        {
            SpecialType.System_Int32 => Resolve(compilation, "System.Int32"),
            SpecialType.System_String => Resolve(compilation, "System.String"),
            SpecialType.System_Boolean => Resolve(compilation, "System.Boolean"),
            SpecialType.System_Object => Resolve(compilation, "System.Object"),
            SpecialType.System_Void => Resolve(compilation, "System.Void"),
            SpecialType.System_Unit => Resolve(compilation, "System.Void"),
            SpecialType.System_Double => Resolve(compilation, "System.Double"),
            SpecialType.System_Char => Resolve(compilation, "System.Char"),
            SpecialType.System_Int64 => Resolve(compilation, "System.Int64"),
            SpecialType.System_Single => Resolve(compilation, "System.Single"),
            SpecialType.System_Byte => Resolve(compilation, "System.Byte"),
            SpecialType.System_Decimal => Resolve(compilation, "System.Decimal"),
            SpecialType.System_Int16 => Resolve(compilation, "System.Int16"),
            SpecialType.System_UInt32 => Resolve(compilation, "System.UInt32"),
            SpecialType.System_UInt64 => Resolve(compilation, "System.UInt64"),
            SpecialType.System_UInt16 => Resolve(compilation, "System.UInt16"),
            SpecialType.System_SByte => Resolve(compilation, "System.SByte"),
            SpecialType.System_DateTime => Resolve(compilation, "System.DateTime"),
            SpecialType.System_Array => Resolve(compilation, "System.Array"),
            SpecialType.System_Type => Resolve(compilation, "System.Type"),
            _ => throw new NotSupportedException($"Unsupported special type: {specialType}")
        };
    }
}
