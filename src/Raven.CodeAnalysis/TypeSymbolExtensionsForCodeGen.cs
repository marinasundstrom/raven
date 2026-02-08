using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using static Raven.CodeAnalysis.CodeGen.DebugUtils;

namespace Raven.CodeAnalysis;

public static class TypeSymbolExtensionsForCodeGen
{
    internal static Type GetClrType(ITypeSymbol typeSymbol, CodeGenerator codeGen)
        => GetClrTypeInternal(
            typeSymbol,
            codeGen,
            treatUnitAsVoid: false,
            usage: RuntimeTypeUsage.Signature,
            isTopLevel: true,
            visiting: new HashSet<ITypeSymbol>(ReferenceEqualityComparer.Instance));

    internal static Type GetClrTypeTreatingUnitAsVoid(ITypeSymbol typeSymbol, CodeGenerator codeGen)
        => GetClrTypeInternal(
            typeSymbol,
            codeGen,
            treatUnitAsVoid: true,
            usage: RuntimeTypeUsage.Signature,
            isTopLevel: true,
            visiting: new HashSet<ITypeSymbol>(ReferenceEqualityComparer.Instance));

    internal static Type GetClrTypeTreatingUnitAsVoidForMethodBody(ITypeSymbol typeSymbol, CodeGenerator codeGen)
        => GetClrTypeInternal(
            typeSymbol,
            codeGen,
            treatUnitAsVoid: true,
            usage: RuntimeTypeUsage.MethodBody,
            isTopLevel: true,
            visiting: new HashSet<ITypeSymbol>(ReferenceEqualityComparer.Instance));

    private static Type GetClrTypeInternal(
        ITypeSymbol typeSymbol,
        CodeGenerator codeGen,
        bool treatUnitAsVoid,
        RuntimeTypeUsage usage,
        bool isTopLevel,
        HashSet<ITypeSymbol> visiting)
    {
        if (typeSymbol == null)
            throw new ArgumentNullException(nameof(typeSymbol));
        if (codeGen == null)
            throw new ArgumentNullException(nameof(codeGen));

        if (!visiting.Add(typeSymbol))
            return ResolveCycleFallback(typeSymbol, codeGen, treatUnitAsVoid, isTopLevel);

        try
        {
            if (typeSymbol.IsAlias && typeSymbol is IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasUnderlying })
            {
                return GetClrTypeInternal(aliasUnderlying, codeGen, treatUnitAsVoid, usage, isTopLevel, visiting);
            }

            var compilation = codeGen.Compilation;
            if (typeSymbol is ConstructedNamedTypeSymbol constructedType)
            {
                if (constructedType.ConstructedFrom is not INamedTypeSymbol definition)
                    throw new InvalidOperationException("Constructed type is missing its original definition.");

                var genericDefinition = GetClrTypeInternal(definition, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
                var explicitArguments = constructedType.GetExplicitTypeArgumentsForInference();
                var directArguments = constructedType.TypeArguments;
                var allArguments = constructedType.GetAllTypeArguments();
                var preferredArguments = SelectRuntimeTypeArguments(
                    genericDefinition,
                    explicitArguments,
                    directArguments,
                    allArguments);
                ImmutableArray<ITypeSymbol> symbolArguments = preferredArguments;
                Type[] arguments;

                if (CodeGenFlags.PrintDebug)
                {
                    PrintDebug(
                        $"[CodeGen:Type] Resolve constructed type {constructedType} from {definition} " +
                        $"(explicitArgs={explicitArguments.Length}, allArgs={allArguments.Length}, chosen={preferredArguments.Length})");
                }

                try
                {
                    arguments = preferredArguments
                            .Select(arg => GetClrTypeInternal(arg, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting))
                        .ToArray();
                }
                catch (InvalidOperationException ex) when (ex.Message.StartsWith("Unable to resolve runtime type for type parameter:", StringComparison.Ordinal))
                {
                    // Some metadata constructs can retain method type parameters in explicit arguments.
                    // Retry with normalized arguments that may already be substituted in this context.
                    var normalizedArguments = constructedType.TypeArguments;
                    if (normalizedArguments.IsDefaultOrEmpty || normalizedArguments.Length != preferredArguments.Length)
                        throw;

                    symbolArguments = normalizedArguments;
                    arguments = normalizedArguments
                        .Select(arg => GetClrTypeInternal(arg, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting))
                        .ToArray();

                    if (CodeGenFlags.PrintDebug)
                    {
                        PrintDebug(
                            $"[CodeGen:Type] Fallback normalized arguments for {constructedType} " +
                            $"(normalizedCount={normalizedArguments.Length})");
                    }
                }

                if (CodeGenFlags.PrintDebug)
                {
                    var debugArgs = symbolArguments.Select(a => a?.ToDisplayString() ?? "<null>");
                    PrintDebug(
                        $"[CodeGen:Type] Projected runtime type {genericDefinition} with args [{string.Join(", ", debugArgs)}] -> " +
                        $"[{string.Join(", ", arguments.Select(a => a?.FullName ?? a?.ToString() ?? "<null>"))}]");
                }

                if (!genericDefinition.IsGenericTypeDefinition && !genericDefinition.ContainsGenericParameters)
                    return genericDefinition;

                if (arguments.Length == 0)
                    return genericDefinition;

                return genericDefinition.MakeGenericType(arguments);
            }

            if (typeSymbol is NullableTypeSymbol nullableType)
            {
                var underlying = GetClrTypeInternal(nullableType.UnderlyingType, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
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
                if (codeGen.RuntimeTypeMap.TryResolveTypeParameter(typeParameterSymbol, usage, out var parameterType))
                    return parameterType;

                if (TryResolveRuntimeTypeParameterFallback(typeParameterSymbol, codeGen, out var fallbackType))
                    return fallbackType;

                var original = typeParameterSymbol.OriginalDefinition as ITypeParameterSymbol;
                throw new InvalidOperationException(
                    $"Unable to resolve runtime type for type parameter: {typeParameterSymbol.Name}, in {typeParameterSymbol.ContainingSymbol} " +
                    $"[symbolType={typeParameterSymbol.GetType().Name}, ordinal={typeParameterSymbol.Ordinal}, " +
                    $"originalType={(original?.GetType().Name ?? "<null>")}, originalContaining={(original?.ContainingSymbol?.ToString() ?? "<null>")}]");
            }

        if (typeSymbol is IArrayTypeSymbol arrayType)
        {
            var elementClrType = GetClrTypeInternal(arrayType.ElementType, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
            return arrayType.Rank == 1
                ? elementClrType.MakeArrayType()
                : elementClrType.MakeArrayType(arrayType.Rank);
        }

        if (typeSymbol is ByRefTypeSymbol byRefType)
        {
            var elementClrType = GetClrTypeInternal(byRefType.ElementType, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
            return elementClrType.MakeByRefType();
        }

        if (typeSymbol is IPointerTypeSymbol pointerType)
        {
            var elementClrType = GetClrTypeInternal(pointerType.PointedAtType, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
            return elementClrType.MakePointerType();
        }

        if (typeSymbol is ITupleTypeSymbol tupleSymbol)
        {
            var elementClrTypes = tupleSymbol.TupleElements
                .Select(e => GetClrTypeInternal(e.Type, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting))
                .ToArray();

            return GetValueTupleClrType(elementClrTypes, compilation);
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
            return GetClrTypeInternal(literalType.UnderlyingType, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
        }

        if (typeSymbol is INamedTypeSymbol named &&
            named is not ConstructedNamedTypeSymbol &&
            named.IsGenericType &&
            !named.IsUnboundGenericType)
        {
            if (named.ConstructedFrom is INamedTypeSymbol definition &&
                !ReferenceEquals(named, definition))
            {
                var genericDef = GetClrTypeInternal(definition, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);
                var args = named.TypeArguments
                    .Select(arg => GetClrTypeInternal(arg, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting))
                    .ToArray();

                if (!genericDef.IsGenericTypeDefinition && !genericDef.ContainsGenericParameters)
                {
                    return genericDef;
                }

                return genericDef.MakeGenericType(args);
            }
        }

        if (typeSymbol.SpecialType != SpecialType.None)
        {
            return GetSpecialClrType(typeSymbol.SpecialType, compilation);
        }

        if (typeSymbol is INamedTypeSymbol namedType)
        {
            if (codeGen.TryEnsureRuntimeTypeForSymbol(namedType, out var builtType))
                return builtType;

            var metadataName = namedType.ToFullyQualifiedMetadataName();
            var runtimeType = compilation.ResolveRuntimeType(metadataName);
            if (runtimeType is not null)
                return runtimeType;

            throw new InvalidOperationException($"Unable to resolve runtime type for symbol: {metadataName}");
        }

        if (typeSymbol is ITypeUnionSymbol union)
        {
            var emission = union.GetUnionEmissionInfo(compilation);
            var underlyingClr = GetClrTypeInternal(emission.UnderlyingTypeSymbol, codeGen, treatUnitAsVoid, usage, isTopLevel: false, visiting);

            if (emission.WrapInNullable)
            {
                var nullableDefinition = GetNullableRuntimeType(compilation);
                return nullableDefinition.MakeGenericType(underlyingClr);
            }

            return underlyingClr;
        }

            throw new NotSupportedException($"Unsupported type symbol: {typeSymbol}");
        }
        finally
        {
            visiting.Remove(typeSymbol);
        }
    }

    private static ImmutableArray<ITypeSymbol> SelectRuntimeTypeArguments(
        Type genericDefinition,
        ImmutableArray<ITypeSymbol> explicitArguments,
        ImmutableArray<ITypeSymbol> directArguments,
        ImmutableArray<ITypeSymbol> allArguments)
    {
        var candidates = new[]
        {
            explicitArguments,
            directArguments,
            allArguments
        };

        if (genericDefinition.IsGenericTypeDefinition || genericDefinition.ContainsGenericParameters)
        {
            var expectedArity = genericDefinition.GetGenericArguments().Length;
            foreach (var candidate in candidates)
            {
                if (candidate.IsDefaultOrEmpty)
                    continue;

                if (candidate.Length == expectedArity)
                    return candidate;
            }
        }

        foreach (var candidate in candidates)
        {
            if (!candidate.IsDefaultOrEmpty)
                return candidate;
        }

        return ImmutableArray<ITypeSymbol>.Empty;
    }

    private static Type ResolveCycleFallback(ITypeSymbol typeSymbol, CodeGenerator codeGen, bool treatUnitAsVoid, bool isTopLevel)
    {
        if (CodeGenFlags.PrintDebug)
            PrintDebug($"[CodeGen:Type] Cycle fallback for {typeSymbol}");

        if (typeSymbol.SpecialType != SpecialType.None)
            return GetSpecialClrType(typeSymbol.SpecialType, codeGen.Compilation);

        if (typeSymbol is ITypeParameterSymbol typeParameter &&
            (codeGen.TryGetRuntimeTypeForTypeParameter(typeParameter, out var parameterType)
             || TryResolveRuntimeTypeParameterFallback(typeParameter, codeGen, out parameterType)))
        {
            return parameterType;
        }

        if (typeSymbol is INamedTypeSymbol named)
        {
            if (codeGen.TryEnsureRuntimeTypeForSymbol(named, out var builtType))
                return builtType;

            if (named.ConstructedFrom is INamedTypeSymbol definition && !ReferenceEquals(definition, named))
            {
                if (codeGen.TryEnsureRuntimeTypeForSymbol(definition, out var definitionType))
                    return definitionType;
            }
        }

        return ResolveUnitRuntimeType(typeSymbol, codeGen.Compilation, codeGen, treatUnitAsVoid, isTopLevel);
    }

    private static bool TryResolveRuntimeTypeParameterFallback(
        ITypeParameterSymbol symbol,
        CodeGenerator codeGen,
        out Type type)
    {
        if (symbol.OwnerKind == TypeParameterOwnerKind.Type &&
            symbol.DeclaringTypeParameterOwner is INamedTypeSymbol containingType)
        {
            var runtimeType = GetClrTypeTreatingUnitAsVoid(containingType, codeGen);
            if (runtimeType.IsGenericType)
            {
                // Prefer constructed arguments when available (e.g. Option<!!T>), otherwise fall back
                // to generic definition parameters (e.g. Option<!0>).
                var constructedArguments = runtimeType.GetGenericArguments();
                if ((uint)symbol.Ordinal < (uint)constructedArguments.Length)
                {
                    type = constructedArguments[symbol.Ordinal];
                    PrintDebug($"[CodeGen:TypeParam] Fallback resolved {symbol.Name} from constructed containing type {runtimeType} => {type}");
                    return true;
                }

                var definition = runtimeType.IsGenericTypeDefinition
                    ? runtimeType
                    : runtimeType.GetGenericTypeDefinition();
                var definitionParameters = definition.GetGenericArguments();
                if ((uint)symbol.Ordinal < (uint)definitionParameters.Length)
                {
                    type = definitionParameters[symbol.Ordinal];
                    PrintDebug($"[CodeGen:TypeParam] Fallback resolved {symbol.Name} from generic definition {definition} => {type}");
                    return true;
                }
            }
        }

        type = null!;
        return false;
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

    internal static Type GetValueTupleClrType(Type[] elementClrTypes, Compilation compilation)
    {
        if (elementClrTypes.Length == 0)
            return ResolveRuntimeTypeOrThrow(compilation, "System.ValueTuple");

        if (elementClrTypes.Length <= 7)
        {
            var metadataName = $"System.ValueTuple`{elementClrTypes.Length}";
            var definition = ResolveRuntimeTypeOrThrow(compilation, metadataName);
            return definition.MakeGenericType(elementClrTypes);
        }

        var restLength = elementClrTypes.Length - 7;
        var restTypes = new Type[restLength];
        Array.Copy(elementClrTypes, 7, restTypes, 0, restLength);
        var restTuple = GetValueTupleClrType(restTypes, compilation);

        var args = new Type[8];
        Array.Copy(elementClrTypes, args, 7);
        args[7] = restTuple;

        var valueTuple8 = ResolveRuntimeTypeOrThrow(compilation, "System.ValueTuple`8");
        return valueTuple8.MakeGenericType(args);
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

    internal static UnionEmissionInfo GetUnionEmissionInfo(this ITypeUnionSymbol union, Compilation compilation)
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

        var declaredUnderlying = union.DeclaredUnderlyingType;
        var wrapInNullable = false;
        ITypeSymbol underlying;

        if (declaredUnderlying is NullableTypeSymbol nullableDeclared)
        {
            underlying = nullableDeclared.UnderlyingType;
            wrapInNullable = true;
        }
        else if (declaredUnderlying is not null)
        {
            underlying = declaredUnderlying;
            if (includesNull && declaredUnderlying.IsValueType)
            {
                // Value types cannot represent null unless explicitly nullable.
                // Fall back to wrapping in Nullable so that metadata signatures remain valid.
                wrapInNullable = true;
            }
        }
        else
        {
            var common = FindCommonDenominator(nonNull);
            underlying = common ?? compilation.GetSpecialType(SpecialType.System_Object);

            if (includesNull && distinctNonNull.Length == 1 && distinctNonNull[0].IsValueType)
            {
                underlying = distinctNonNull[0];
                wrapInNullable = true;
            }
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
        if (type is ITypeUnionSymbol union)
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
