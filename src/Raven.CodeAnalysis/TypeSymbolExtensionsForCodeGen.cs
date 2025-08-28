using System.Collections.Generic;
using System.Linq;
using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensionsForCodeGen
{
    /// <summary>
    /// Resolves the CLR Type from an ITypeSymbol in the context of CodeGen (which handles both metadata and emitted types).
    /// </summary>
    internal static Type GetClrType(this ITypeSymbol typeSymbol, CodeGenerator codeGen)
    {
        if (typeSymbol == null)
            throw new ArgumentNullException(nameof(typeSymbol));
        if (codeGen == null)
            throw new ArgumentNullException(nameof(codeGen));

        if (typeSymbol is NullableTypeSymbol nullableType)
        {
            var underlying = nullableType.UnderlyingType.GetClrType(codeGen);
            if (nullableType.UnderlyingType.IsValueType)
                return typeof(Nullable<>).MakeGenericType(underlying);
            return underlying;
        }

        if (typeSymbol is PENamedTypeSymbol namedTypeSymbol)
        {
            return namedTypeSymbol.GetTypeInfo();
        }

        var compilation = codeGen.Compilation;

        // Handle arrays
        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var elementClrType = arrayType.ElementType.GetClrType(codeGen);
            return elementClrType.MakeArrayType(); // TODO: support Rank > 1
        }

        // Handle arrays
        if (typeSymbol is ITupleTypeSymbol tupleSymbol)
        {
            var tupleClrType = tupleSymbol.UnderlyingTupleType.GetClrType(codeGen);
            return tupleClrType.MakeGenericType(tupleSymbol.TupleElements.Select(e => e.Type.GetClrType(codeGen)).ToArray());
        }

        if (typeSymbol is NullTypeSymbol)
        {
            if (codeGen.NullType is null)
                throw new InvalidOperationException("Null type was not emitted.");
            return codeGen.NullType;
        }

        if (typeSymbol.SpecialType == SpecialType.System_Unit)
        {
            if (codeGen.UnitType is null)
                throw new InvalidOperationException("Unit type was not emitted.");
            return codeGen.UnitType;
        }

        if (typeSymbol is LiteralTypeSymbol literalType)
        {
            return literalType.UnderlyingType.GetClrType(codeGen);
        }

        /*
        // Handle pointer types
        if (typeSymbol is IPointerTypeSymbol pointerType)
        {
            var pointedType = pointerType.PointedAtType.GetClrType(codeGen);
            return pointedType.MakePointerType();
        }
        */

        // Handle special types like int, string, etc.
        if (typeSymbol.SpecialType != SpecialType.None)
        {
            return GetSpecialClrType(typeSymbol.SpecialType, compilation);
        }

        // Handle constructed generic types (from metadata or emitted)
        if (typeSymbol is INamedTypeSymbol named && named.IsGenericType && !named.IsUnboundGenericType)
        {
            var genericDef = named.ConstructedFrom.GetClrType(codeGen);
            var args = named.TypeArguments.Select(arg => arg.GetClrType(codeGen)).ToArray();
            return genericDef.MakeGenericType(args);
        }

        // Handle regular named types
        if (typeSymbol is INamedTypeSymbol namedType)
        {
            // Check if CodeGen knows this symbol (i.e., it's being emitted by us)
            if (codeGen.TryGetRuntimeTypeForSymbol(namedType, out var builtType))
                return builtType;

            // Otherwise, attempt to resolve from metadata (reference assemblies)
            var fullyQualifiedName = namedType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var metadataType = compilation.CoreAssembly.GetType(fullyQualifiedName, throwOnError: false);
            if (metadataType != null)
                return metadataType;

            throw new InvalidOperationException($"Unable to resolve runtime type for symbol: {fullyQualifiedName}");
        }

        // Handle union types
        if (typeSymbol is IUnionTypeSymbol union)
        {
            var nonNull = union.Types.Where(t => t.TypeKind != TypeKind.Null).ToArray();

            var common = FindCommonDenominator(nonNull);
            if (common is null)
                common = compilation.GetSpecialType(SpecialType.System_Object);

            var clr = common.GetClrType(codeGen);

            if (union.Types.Any(t => t.TypeKind == TypeKind.Null) && common.IsValueType)
                return typeof(Nullable<>).MakeGenericType(clr);

            return clr;
        }

        throw new NotSupportedException($"Unsupported type symbol: {typeSymbol}");
    }

    private static Type GetSpecialClrType(SpecialType specialType, Compilation compilation)
    {
        static Type FromCoreAssembly(Compilation c, string fullName) =>
            c.CoreAssembly.GetType(fullName)
            ?? throw new InvalidOperationException($"Type '{fullName}' not found in CoreAssembly");

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
            SpecialType.System_ValueTuple_T1 => FromCoreAssembly(compilation, "System.ValueTuple`1"),
            SpecialType.System_ValueTuple_T2 => FromCoreAssembly(compilation, "System.ValueTuple`2"),
            _ => throw new NotSupportedException($"Unsupported special type: {specialType}")
        };
    }

    private static INamedTypeSymbol? FindCommonDenominator(IEnumerable<ITypeSymbol> types)
    {
        HashSet<INamedTypeSymbol>? intersection = null;

        foreach (var type in types)
        {
            if (type is not INamedTypeSymbol named)
                return null;

            var candidates = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

            for (INamedTypeSymbol? current = named; current is not null; current = current.BaseType)
                candidates.Add(current);

            foreach (var iface in named.AllInterfaces)
                candidates.Add(iface);

            if (intersection is null)
                intersection = candidates;
            else
                intersection.IntersectWith(candidates);

            if (intersection.Count == 0)
                return null;
        }

        if (intersection is null || intersection.Count == 0)
            return null;

        return intersection
            .OrderByDescending(GetDepth)
            .ThenByDescending(t => t.TypeKind == TypeKind.Interface ? 1 : 0)
            .First();
    }

    private static int GetDepth(INamedTypeSymbol type)
    {
        int depth = 0;
        for (INamedTypeSymbol? current = type; current is not null; current = current.BaseType)
            depth++;
        return depth;
    }
}
