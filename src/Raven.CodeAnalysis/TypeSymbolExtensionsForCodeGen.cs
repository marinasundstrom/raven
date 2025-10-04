using System.Collections.Generic;
using System.Collections.Immutable;
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
            if (!nullableType.UnderlyingType.IsValueType)
                return underlying;

            var nullableDefinition = GetMetadataNullableType(codeGen.Compilation);
            return nullableDefinition.MakeGenericType(underlying);
        }

        if (typeSymbol is PENamedTypeSymbol namedTypeSymbol)
        {
            return namedTypeSymbol.GetTypeInfo();
        }

        if (typeSymbol is ITypeParameterSymbol typeParameterSymbol)
        {
            if (codeGen.TryGetRuntimeTypeForTypeParameter(typeParameterSymbol, out var parameterType))
                return parameterType;

            if (typeParameterSymbol is PETypeParameterSymbol peTypeParameter)
                return peTypeParameter.GetTypeInfo();

            throw new InvalidOperationException($"Unable to resolve runtime type for type parameter: {typeParameterSymbol.Name}");
        }

        var compilation = codeGen.Compilation;

        // Handle arrays
        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var elementClrType = arrayType.ElementType.GetClrType(codeGen);
            return arrayType.Rank == 1
                ? elementClrType.MakeArrayType()
                : elementClrType.MakeArrayType(arrayType.Rank);
        }

        if (typeSymbol is ByRefTypeSymbol byRefType)
        {
            var elementClrType = byRefType.ElementType.GetClrType(codeGen);
            return elementClrType.MakeByRefType();
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
        if (typeSymbol is ConstructedNamedTypeSymbol constructedNamed)
        {
            var definition = constructedNamed.ConstructedFrom as INamedTypeSymbol
                ?? throw new InvalidOperationException("Constructed type without named definition.");

            var genericDef = definition.GetClrType(codeGen);
            var args = constructedNamed.TypeArguments.Select(arg => arg.GetClrType(codeGen)).ToArray();

            if (!genericDef.IsGenericTypeDefinition && !genericDef.ContainsGenericParameters)
            {
                // The definition is already fully constructed (e.g. metadata bug). Just return it.
                return genericDef;
            }

            return genericDef.MakeGenericType(args);
        }

        if (typeSymbol is INamedTypeSymbol named &&
            named.IsGenericType &&
            !named.IsUnboundGenericType)
        {
            if (named.ConstructedFrom is INamedTypeSymbol definition &&
                !ReferenceEquals(named, definition))
            {
                var genericDef = definition.GetClrType(codeGen);
                var args = named.TypeArguments.Select(arg => arg.GetClrType(codeGen)).ToArray();

                if (!genericDef.IsGenericTypeDefinition && !genericDef.ContainsGenericParameters)
                {
                    return genericDef;
                }

                return genericDef.MakeGenericType(args);
            }
        }

        // Handle regular named types
        if (typeSymbol is INamedTypeSymbol namedType)
        {
            // Check if CodeGen knows this symbol (i.e., it's being emitted by us)
            if (codeGen.TryGetRuntimeTypeForSymbol(namedType, out var builtType))
                return builtType;

            // Otherwise, attempt to resolve from metadata (reference assemblies)
            var metadataName = namedType.ToFullyQualifiedMetadataName();
            var metadataType = compilation.CoreAssembly.GetType(metadataName, throwOnError: false);
            if (metadataType != null)
                return metadataType;

            throw new InvalidOperationException($"Unable to resolve runtime type for symbol: {metadataName}");
        }

        // Handle union types
        if (typeSymbol is IUnionTypeSymbol union)
        {
            var emission = union.GetUnionEmissionInfo(compilation);
            var underlyingClr = emission.UnderlyingTypeSymbol.GetClrType(codeGen);

            if (emission.WrapInNullable)
            {
                var nullableDefinition = GetMetadataNullableType(compilation);
                return nullableDefinition.MakeGenericType(underlyingClr);
            }

            return underlyingClr;
        }

        throw new NotSupportedException($"Unsupported type symbol: {typeSymbol}");
    }

    private static Type GetMetadataNullableType(Compilation compilation)
    {
        return compilation.CoreAssembly.GetType("System.Nullable`1")
            ?? throw new InvalidOperationException("System.Nullable`1 not found in the core assembly.");
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
            SpecialType.System_ValueTuple_T3 => FromCoreAssembly(compilation, "System.ValueTuple`3"),
            SpecialType.System_ValueTuple_T4 => FromCoreAssembly(compilation, "System.ValueTuple`4"),
            SpecialType.System_ValueTuple_T5 => FromCoreAssembly(compilation, "System.ValueTuple`5"),
            SpecialType.System_ValueTuple_T6 => FromCoreAssembly(compilation, "System.ValueTuple`6"),
            SpecialType.System_ValueTuple_T7 => FromCoreAssembly(compilation, "System.ValueTuple`7"),
            SpecialType.System_ValueTuple_TRest => FromCoreAssembly(compilation, "System.ValueTuple`8"),
            _ => throw new NotSupportedException($"Unsupported special type: {specialType}")
        };
    }

    internal static UnionEmissionInfo GetUnionEmissionInfo(this IUnionTypeSymbol union, Compilation compilation)
    {
        if (union is null)
            throw new ArgumentNullException(nameof(union));
        if (compilation is null)
            throw new ArgumentNullException(nameof(compilation));

        var flattened = FlattenUnionMembers(union).ToImmutableArray();
        var includesNull = flattened.Any(t => t.TypeKind == TypeKind.Null);
        var nonNull = flattened.Where(t => t.TypeKind != TypeKind.Null).ToImmutableArray();

        var distinctNonNull = nonNull
            .Distinct<ITypeSymbol>(SymbolEqualityComparer.Default)
            .ToImmutableArray();

        var common = FindCommonDenominator(nonNull);
        ITypeSymbol underlying = common ?? compilation.GetSpecialType(SpecialType.System_Object);
        var wrapInNullable = false;

        if (includesNull && distinctNonNull.Length == 1 && distinctNonNull[0].IsValueType)
        {
            underlying = distinctNonNull[0];
            wrapInNullable = true;
        }

        return new UnionEmissionInfo(
            underlying,
            wrapInNullable,
            includesNull,
            distinctNonNull);
    }

    internal static INamedTypeSymbol? FindCommonDenominator(IEnumerable<ITypeSymbol> types)
    {
        var namedTypes = types.Select(Unalias).OfType<INamedTypeSymbol>().ToArray();
        if (namedTypes.Length == 0)
            return null;

        INamedTypeSymbol? candidate = namedTypes[0];
        INamedTypeSymbol? objectType = null;
        while (candidate is not null)
        {
            if (namedTypes.All(t => SharesAncestor(t, candidate)))
            {
                if (candidate.SpecialType == SpecialType.System_Object)
                    objectType = candidate;
                else
                    return candidate;
            }
            candidate = candidate.BaseType;
        }

        foreach (var iface in namedTypes[0].AllInterfaces)
        {
            if (namedTypes.All(t => t.AllInterfaces.Contains(iface, SymbolEqualityComparer.Default)))
                return iface;
        }

        return objectType;

        static bool SharesAncestor(INamedTypeSymbol type, INamedTypeSymbol ancestor)
        {
            for (INamedTypeSymbol? current = type; current is not null; current = current.BaseType)
                if (SymbolEqualityComparer.Default.Equals(current, ancestor))
                    return true;
            foreach (var iface in type.AllInterfaces)
                if (SymbolEqualityComparer.Default.Equals(iface, ancestor))
                    return true;
            return false;
        }
    }

    private static IEnumerable<ITypeSymbol> FlattenUnionMembers(ITypeSymbol type)
    {
        if (type is IUnionTypeSymbol union)
        {
            foreach (var member in union.Types)
            {
                foreach (var nested in FlattenUnionMembers(member))
                    yield return nested;
            }

            yield break;
        }

        yield return Unalias(type);
    }

    private static ITypeSymbol Unalias(ITypeSymbol type)
    {
        while (true)
        {
            if (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol t)
            {
                type = t;
                continue;
            }

            if (type is LiteralTypeSymbol lit)
            {
                type = lit.UnderlyingType;
                continue;
            }

            return type;
        }
    }

    private static int GetDepth(INamedTypeSymbol type)
    {
        int depth = 0;
        for (INamedTypeSymbol? current = type; current is not null; current = current.BaseType)
            depth++;
        return depth;
    }
}

internal readonly struct UnionEmissionInfo
{
    public UnionEmissionInfo(
        ITypeSymbol underlyingTypeSymbol,
        bool wrapInNullable,
        bool includesNull,
        ImmutableArray<ITypeSymbol> distinctNonNullMembers)
    {
        UnderlyingTypeSymbol = underlyingTypeSymbol;
        WrapInNullable = wrapInNullable;
        IncludesNull = includesNull;
        DistinctNonNullMembers = distinctNonNullMembers;
    }

    public ITypeSymbol UnderlyingTypeSymbol { get; }

    public bool WrapInNullable { get; }

    public bool IncludesNull { get; }

    public ImmutableArray<ITypeSymbol> DistinctNonNullMembers { get; }
}
