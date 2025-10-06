using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensionsForCodeGen
{
    internal static Type GetClrType(this ITypeSymbol typeSymbol, CodeGenerator codeGen)
        => GetClrTypeInternal(typeSymbol, codeGen, treatUnitAsVoid: false, isTopLevel: true);

    internal static Type GetClrTypeTreatingUnitAsVoid(this ITypeSymbol typeSymbol, CodeGenerator codeGen)
        => GetClrTypeInternal(typeSymbol, codeGen, treatUnitAsVoid: true, isTopLevel: true);

    private static Type GetClrTypeInternal(ITypeSymbol typeSymbol, CodeGenerator codeGen, bool treatUnitAsVoid, bool isTopLevel)
    {
        if (typeSymbol == null)
            throw new ArgumentNullException(nameof(typeSymbol));
        if (codeGen == null)
            throw new ArgumentNullException(nameof(codeGen));

        var compilation = codeGen.Compilation;

        if (typeSymbol is NullableTypeSymbol nullableType)
        {
            var underlying = GetClrTypeInternal(nullableType.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false);
            if (!nullableType.UnderlyingType.IsValueType)
                return underlying;

            var nullableDefinition = GetNullableRuntimeType(codeGen.Compilation);
            return nullableDefinition.MakeGenericType(underlying);
        }

        if (typeSymbol is PENamedTypeSymbol namedTypeSymbol)
        {
            var runtimeType = compilation.ResolveRuntimeType(namedTypeSymbol);
            if (runtimeType is not null)
                return runtimeType;

            if (namedTypeSymbol.SpecialType is not SpecialType.None)
                return GetSpecialClrType(namedTypeSymbol.SpecialType, compilation);

            var metadataName = ((INamedTypeSymbol)namedTypeSymbol).ToFullyQualifiedMetadataName();
            throw new InvalidOperationException($"Unable to resolve runtime type for metadata symbol: {metadataName}");
        }

        if (typeSymbol is ITypeParameterSymbol typeParameterSymbol)
        {
            if (codeGen.TryGetRuntimeTypeForTypeParameter(typeParameterSymbol, out var parameterType))
                return parameterType;

            if (typeParameterSymbol is PETypeParameterSymbol peTypeParameter)
                return peTypeParameter.GetTypeInfo();

            throw new InvalidOperationException($"Unable to resolve runtime type for type parameter: {typeParameterSymbol.Name}");
        }

        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var elementClrType = GetClrTypeInternal(arrayType.ElementType, codeGen, treatUnitAsVoid, isTopLevel: false);
            return arrayType.Rank == 1
                ? elementClrType.MakeArrayType()
                : elementClrType.MakeArrayType(arrayType.Rank);
        }

        if (typeSymbol is ByRefTypeSymbol byRefType)
        {
            var elementClrType = GetClrTypeInternal(byRefType.ElementType, codeGen, treatUnitAsVoid, isTopLevel: false);
            return elementClrType.MakeByRefType();
        }

        if (typeSymbol is ITupleTypeSymbol tupleSymbol)
        {
            var tupleClrType = GetClrTypeInternal(tupleSymbol.UnderlyingTupleType, codeGen, treatUnitAsVoid, isTopLevel: false);
            return tupleClrType.MakeGenericType(tupleSymbol.TupleElements
                .Select(e => GetClrTypeInternal(e.Type, codeGen, treatUnitAsVoid, isTopLevel: false))
                .ToArray());
        }

        if (typeSymbol is NullTypeSymbol)
        {
            if (codeGen.NullType is null)
                throw new InvalidOperationException("Null type was not emitted.");
            return codeGen.NullType;
        }

        if (typeSymbol.SpecialType == SpecialType.System_Unit)
            return ResolveUnitRuntimeType(typeSymbol, compilation, codeGen, treatUnitAsVoid, isTopLevel);

        if (typeSymbol is LiteralTypeSymbol literalType)
        {
            return GetClrTypeInternal(literalType.UnderlyingType, codeGen, treatUnitAsVoid, isTopLevel: false);
        }

        if (typeSymbol.SpecialType != SpecialType.None)
        {
            return GetSpecialClrType(typeSymbol.SpecialType, compilation);
        }

        if (typeSymbol is ConstructedNamedTypeSymbol constructedNamed)
        {
            var definition = constructedNamed.ConstructedFrom as INamedTypeSymbol
                ?? throw new InvalidOperationException("Constructed type without named definition.");

            var genericDef = GetClrTypeInternal(definition, codeGen, treatUnitAsVoid, isTopLevel: false);
            var args = constructedNamed.TypeArguments
                .Select(arg => GetClrTypeInternal(arg, codeGen, treatUnitAsVoid, isTopLevel: false))
                .ToArray();

            if (!genericDef.IsGenericTypeDefinition && !genericDef.ContainsGenericParameters)
            {
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
                var genericDef = GetClrTypeInternal(definition, codeGen, treatUnitAsVoid, isTopLevel: false);
                var args = named.TypeArguments
                    .Select(arg => GetClrTypeInternal(arg, codeGen, treatUnitAsVoid, isTopLevel: false))
                    .ToArray();

                if (!genericDef.IsGenericTypeDefinition && !genericDef.ContainsGenericParameters)
                {
                    return genericDef;
                }

                return genericDef.MakeGenericType(args);
            }
        }

        if (typeSymbol is INamedTypeSymbol namedType)
        {
            if (codeGen.TryGetRuntimeTypeForSymbol(namedType, out var builtType))
                return builtType;

            var metadataName = namedType.ToFullyQualifiedMetadataName();
            var runtimeType = compilation.ResolveRuntimeType(metadataName);
            if (runtimeType is not null)
                return runtimeType;

            throw new InvalidOperationException($"Unable to resolve runtime type for symbol: {metadataName}");
        }

        if (typeSymbol is IUnionTypeSymbol union)
        {
            var emission = union.GetUnionEmissionInfo(compilation);
            var underlyingClr = GetClrTypeInternal(emission.UnderlyingTypeSymbol, codeGen, treatUnitAsVoid, isTopLevel: false);

            if (emission.WrapInNullable)
            {
                var nullableDefinition = GetNullableRuntimeType(compilation);
                return nullableDefinition.MakeGenericType(underlyingClr);
            }

            return underlyingClr;
        }

        throw new NotSupportedException($"Unsupported type symbol: {typeSymbol}");
    }

    private static Type GetNullableRuntimeType(Compilation compilation)
    {
        return ResolveRuntimeTypeOrThrow(compilation, "System.Nullable`1");
    }

    private static Type GetSpecialClrType(SpecialType specialType, Compilation compilation)
    {
        static Type FromRuntime(Compilation c, string metadataName) =>
            ResolveRuntimeTypeOrThrow(c, metadataName);

        return specialType switch
        {
            SpecialType.System_Object => FromRuntime(compilation, "System.Object"),
            SpecialType.System_Enum => FromRuntime(compilation, "System.Enum"),
            SpecialType.System_MulticastDelegate => FromRuntime(compilation, "System.MulticastDelegate"),
            SpecialType.System_Delegate => FromRuntime(compilation, "System.Delegate"),
            SpecialType.System_ValueType => FromRuntime(compilation, "System.ValueType"),
            SpecialType.System_Void => FromRuntime(compilation, "System.Void"),
            SpecialType.System_Boolean => FromRuntime(compilation, "System.Boolean"),
            SpecialType.System_Char => FromRuntime(compilation, "System.Char"),
            SpecialType.System_SByte => FromRuntime(compilation, "System.SByte"),
            SpecialType.System_Byte => FromRuntime(compilation, "System.Byte"),
            SpecialType.System_Int16 => FromRuntime(compilation, "System.Int16"),
            SpecialType.System_UInt16 => FromRuntime(compilation, "System.UInt16"),
            SpecialType.System_Int32 => FromRuntime(compilation, "System.Int32"),
            SpecialType.System_UInt32 => FromRuntime(compilation, "System.UInt32"),
            SpecialType.System_Int64 => FromRuntime(compilation, "System.Int64"),
            SpecialType.System_UInt64 => FromRuntime(compilation, "System.UInt64"),
            SpecialType.System_Decimal => FromRuntime(compilation, "System.Decimal"),
            SpecialType.System_Single => FromRuntime(compilation, "System.Single"),
            SpecialType.System_Double => FromRuntime(compilation, "System.Double"),
            SpecialType.System_String => FromRuntime(compilation, "System.String"),
            SpecialType.System_IntPtr => FromRuntime(compilation, "System.IntPtr"),
            SpecialType.System_UIntPtr => FromRuntime(compilation, "System.UIntPtr"),
            SpecialType.System_Array => FromRuntime(compilation, "System.Array"),
            SpecialType.System_Type => FromRuntime(compilation, "System.Type"),
            SpecialType.System_DateTime => FromRuntime(compilation, "System.DateTime"),
            SpecialType.System_Collections_IEnumerable => FromRuntime(compilation, "System.Collections.IEnumerable"),
            SpecialType.System_Collections_Generic_IEnumerable_T => FromRuntime(compilation, "System.Collections.Generic.IEnumerable`1"),
            SpecialType.System_Collections_Generic_IList_T => FromRuntime(compilation, "System.Collections.Generic.IList`1"),
            SpecialType.System_Collections_Generic_ICollection_T => FromRuntime(compilation, "System.Collections.Generic.ICollection`1"),
            SpecialType.System_Collections_IEnumerator => FromRuntime(compilation, "System.Collections.IEnumerator"),
            SpecialType.System_Collections_Generic_IEnumerator_T => FromRuntime(compilation, "System.Collections.Generic.IEnumerator`1"),
            SpecialType.System_Nullable_T => FromRuntime(compilation, "System.Nullable`1"),
            SpecialType.System_Runtime_CompilerServices_IsVolatile => FromRuntime(compilation, "System.Runtime.CompilerServices.IsVolatile"),
            SpecialType.System_IDisposable => FromRuntime(compilation, "System.IDisposable"),
            SpecialType.System_TypedReference => FromRuntime(compilation, "System.TypedReference"),
            SpecialType.System_ArgIterator => FromRuntime(compilation, "System.ArgIterator"),
            SpecialType.System_RuntimeArgumentHandle => FromRuntime(compilation, "System.RuntimeArgumentHandle"),
            SpecialType.System_RuntimeFieldHandle => FromRuntime(compilation, "System.RuntimeFieldHandle"),
            SpecialType.System_RuntimeMethodHandle => FromRuntime(compilation, "System.RuntimeMethodHandle"),
            SpecialType.System_RuntimeTypeHandle => FromRuntime(compilation, "System.RuntimeTypeHandle"),
            SpecialType.System_IAsyncResult => FromRuntime(compilation, "System.IAsyncResult"),
            SpecialType.System_AsyncCallback => FromRuntime(compilation, "System.AsyncCallback"),
            SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder => FromRuntime(compilation, "System.Runtime.CompilerServices.AsyncVoidMethodBuilder"),
            SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder => FromRuntime(compilation, "System.Runtime.CompilerServices.AsyncTaskMethodBuilder"),
            SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T => FromRuntime(compilation, "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1"),
            SpecialType.System_Runtime_CompilerServices_AsyncStateMachineAttribute => FromRuntime(compilation, "System.Runtime.CompilerServices.AsyncStateMachineAttribute"),
            SpecialType.System_Runtime_CompilerServices_IteratorStateMachineAttribute => FromRuntime(compilation, "System.Runtime.CompilerServices.IteratorStateMachineAttribute"),
            SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine => FromRuntime(compilation, "System.Runtime.CompilerServices.IAsyncStateMachine"),
            SpecialType.System_Threading_Tasks_Task => FromRuntime(compilation, "System.Threading.Tasks.Task"),
            SpecialType.System_Threading_Tasks_Task_T => FromRuntime(compilation, "System.Threading.Tasks.Task`1"),
            SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationToken => FromRuntime(compilation, "System.Runtime.InteropServices.WindowsRuntime.EventRegistrationToken"),
            SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationTokenTable_T => FromRuntime(compilation, "System.Runtime.InteropServices.WindowsRuntime.EventRegistrationTokenTable`1"),
            SpecialType.System_Exception => FromRuntime(compilation, "System.Exception"),
            SpecialType.System_ValueTuple_T1 => FromRuntime(compilation, "System.ValueTuple`1"),
            SpecialType.System_ValueTuple_T2 => FromRuntime(compilation, "System.ValueTuple`2"),
            SpecialType.System_ValueTuple_T3 => FromRuntime(compilation, "System.ValueTuple`3"),
            SpecialType.System_ValueTuple_T4 => FromRuntime(compilation, "System.ValueTuple`4"),
            SpecialType.System_ValueTuple_T5 => FromRuntime(compilation, "System.ValueTuple`5"),
            SpecialType.System_ValueTuple_T6 => FromRuntime(compilation, "System.ValueTuple`6"),
            SpecialType.System_ValueTuple_T7 => FromRuntime(compilation, "System.ValueTuple`7"),
            SpecialType.System_ValueTuple_TRest => FromRuntime(compilation, "System.ValueTuple`8"),
            SpecialType.System_Unit => FromRuntime(compilation, "System.Void"),
            _ => throw new NotSupportedException($"Unsupported special type: {specialType}")
        };
    }

    private static Type ResolveRuntimeTypeOrThrow(Compilation compilation, string metadataName)
    {
        return compilation.ResolveRuntimeType(metadataName)
            ?? throw new InvalidOperationException($"Type '{metadataName}' not found in runtime assemblies.");
    }

    private static Type ResolveUnitRuntimeType(
        ITypeSymbol unitSymbol,
        Compilation compilation,
        CodeGenerator codeGen,
        bool treatUnitAsVoid,
        bool isTopLevel)
    {
        if (treatUnitAsVoid && isTopLevel)
            return GetSpecialClrType(SpecialType.System_Void, compilation);

        if (IsCompilationUnitType(unitSymbol, compilation))
        {
            if (codeGen.UnitType is null)
                throw new InvalidOperationException("Unit type was not emitted.");

            return codeGen.UnitType;
        }

        return GetSpecialClrType(SpecialType.System_Void, compilation);
    }

    private static bool IsCompilationUnitType(ITypeSymbol unitSymbol, Compilation compilation)
    {
        if (unitSymbol is UnitTypeSymbol)
            return true;

        if (SymbolEqualityComparer.Default.Equals(unitSymbol, compilation.UnitTypeSymbol))
            return true;

        if (unitSymbol is INamedTypeSymbol named)
        {
            var original = named.OriginalDefinition;
            if (original is not null && SymbolEqualityComparer.Default.Equals(original, compilation.UnitTypeSymbol))
                return true;
        }

        return false;
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
