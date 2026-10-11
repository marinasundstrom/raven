using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeMapper : IEmissionTypeMapper<PrimitiveType>
{
    internal static IEmissionTypeMapper<PrimitiveType> Instance { get; } = new NeoClrTypeMapper();

    internal static SignatureType Map(EmissionType type, Func<INamedTypeSymbol, TypeBuilder> resolveClass, Func<INamedTypeSymbol, SignatureType>? resolveExternal = null, Func<ITypeParameterSymbol, SignatureType>? mapParameter = null)
    {
        if (type.IsByReference) return SignatureType.ByReference(Map(type with { IsByReference = false }, resolveClass, resolveExternal, mapParameter));
        if (type.Pointer is { } pointer) return MapPointer(pointer);
        if (type.OwnerParameter is { } ownerParameter) return mapParameter?.Invoke(ownerParameter) ?? SignatureType.TypeParameter(ownerParameter.Ordinal);
        if (type.MethodParameter is { } parameter) return mapParameter?.Invoke(parameter) ?? SignatureType.MethodParameter(parameter.Ordinal);
        if (type.Primitive is { } p) return Instance.Map(p);
        if (type.Array is { } array)
        {
            CallableSignature.TryType(array.ElementType, false, out var element, NeoClrCapabilities.Shared);
            return SignatureType.ArrayOf(Map(element, resolveClass, resolveExternal, mapParameter));
        }
        var named = type.Nominal!;
        if (named.SpecialType == SpecialType.System_RuntimeTypeHandle) return PrimitiveType.RuntimeTypeHandle;
        if (named.TypeKind == TypeKind.Delegate && CallableSignature.TryFunction(named, out var shape, NeoClrCapabilities.Shared))
            return SignatureType.Function(new MethodSignature(Map(shape.ReturnType, resolveClass, resolveExternal, mapParameter), shape.ParameterTypes.Select(t => Map(t, resolveClass, resolveExternal, mapParameter))));
        if (resolveExternal is not null && (named.TypeKind == TypeKind.Enum && named.DeclaringSyntaxReferences.IsEmpty || CallableSignature.IsExternalReference(named, NeoClrCapabilities.Shared.AllowsNestedExternalTypes, NeoClrCapabilities.Shared.AllowsInternalExternalSymbols) || CallableSignature.IsExternalValue(named, NeoClrCapabilities.Shared.AllowsNestedExternalTypes, NeoClrCapabilities.Shared.AllowsInternalExternalSymbols))) return resolveExternal(named);
        if (named.Arity > 0) return resolveClass((INamedTypeSymbol)named.OriginalDefinition).MakeGenericInstance(named.TypeArguments.Select(t => Map(t, resolveClass, resolveExternal, mapParameter)).ToArray());
        // A payload-free case can be projected through a constructed generic union
        // while its physical case type still has arity zero.
        return resolveClass((INamedTypeSymbol)named.OriginalDefinition);
    }

    internal static SignatureType Map(ITypeSymbol type, Func<INamedTypeSymbol, TypeBuilder> resolveClass, Func<INamedTypeSymbol, SignatureType>? resolveExternal = null, Func<ITypeParameterSymbol, SignatureType>? mapParameter = null)
    {
        if (!CallableSignature.TryType(type, false, out var value, NeoClrCapabilities.Shared)) throw new InvalidOperationException("unsupported native value type");
        return Map(value, resolveClass, resolveExternal, mapParameter);
    }

    private static SignatureType MapPointer(IPointerTypeSymbol pointer)
    {
        if (pointer.PointedAtType is IPointerTypeSymbol nested) return SignatureType.PointerTo(MapPointer(nested));
        if (!EmissionPrimitiveTypes.TryGetReturnType(pointer.PointedAtType, out var primitive))
            throw new InvalidOperationException("unsupported native pointer target");
        return SignatureType.PointerTo(Instance.Map(primitive));
    }

    public PrimitiveType Map(EmissionPrimitiveType type) => type switch
    {
        EmissionPrimitiveType.String => PrimitiveType.String,
        EmissionPrimitiveType.Int32 => PrimitiveType.Int32,
        EmissionPrimitiveType.Byte => PrimitiveType.Byte,
        EmissionPrimitiveType.SByte => PrimitiveType.SByte,
        EmissionPrimitiveType.Int16 => PrimitiveType.Int16,
        EmissionPrimitiveType.UInt16 => PrimitiveType.UInt16,
        EmissionPrimitiveType.UInt32 => PrimitiveType.UInt32,
        EmissionPrimitiveType.UInt64 => PrimitiveType.UInt64,
        EmissionPrimitiveType.IntPtr => PrimitiveType.IntPtr,
        EmissionPrimitiveType.UIntPtr => PrimitiveType.UIntPtr,
        EmissionPrimitiveType.Int64 => PrimitiveType.Int64,
        EmissionPrimitiveType.Single => PrimitiveType.Single,
        EmissionPrimitiveType.Double => PrimitiveType.Double,
        EmissionPrimitiveType.Boolean => PrimitiveType.Boolean,
        EmissionPrimitiveType.NoResult => PrimitiveType.Void,
        _ => throw new ArgumentOutOfRangeException(nameof(type))
    };
}
