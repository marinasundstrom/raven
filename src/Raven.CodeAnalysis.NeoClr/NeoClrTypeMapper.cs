using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeMapper : IEmissionTypeMapper<PrimitiveType>
{
    internal static IEmissionTypeMapper<PrimitiveType> Instance { get; } = new NeoClrTypeMapper();

    internal static SignatureType Map(EmissionType type, Func<INamedTypeSymbol, TypeBuilder> resolveClass, Func<INamedTypeSymbol, SignatureType>? resolveExternal = null)
    {
        if (type.IsByReference) return SignatureType.ByReference(Map(type with { IsByReference = false }, resolveClass, resolveExternal));
        if (type.OwnerParameter is { } ownerParameter) return SignatureType.TypeParameter(ownerParameter.Ordinal);
        if (type.MethodParameter is { } parameter) return SignatureType.MethodParameter(parameter.Ordinal);
        if (type.Primitive is { } p) return Instance.Map(p);
        if (type.Array is { } array)
        {
            CallableSignature.TryType(array.ElementType, false, out var element, NeoClrCapabilities.Shared);
            return SignatureType.ArrayOf(Map(element, resolveClass, resolveExternal));
        }
        var named = type.Nominal!;
        if (named.TypeKind == TypeKind.Delegate && CallableSignature.TryFunction(named, out var shape, NeoClrCapabilities.Shared))
            return SignatureType.Function(new MethodSignature(Map(shape.ReturnType, resolveClass, resolveExternal), shape.ParameterTypes.Select(t => Map(t, resolveClass, resolveExternal))));
        if (resolveExternal is not null && (CallableSignature.IsExternalReference(named, NeoClrCapabilities.Shared.AllowsNestedExternalTypes) || CallableSignature.IsExternalValue(named, NeoClrCapabilities.Shared.AllowsNestedExternalTypes))) return resolveExternal(named);
        if (named.Arity > 0) return resolveClass((INamedTypeSymbol)named.OriginalDefinition).MakeGenericInstance(named.TypeArguments.Select(t => Map(t, resolveClass, resolveExternal)).ToArray());
        // A payload-free case can be projected through a constructed generic union
        // while its physical case type still has arity zero.
        return resolveClass((INamedTypeSymbol)named.OriginalDefinition);
    }

    internal static SignatureType Map(ITypeSymbol type, Func<INamedTypeSymbol, TypeBuilder> resolveClass, Func<INamedTypeSymbol, SignatureType>? resolveExternal = null)
    {
        if (!CallableSignature.TryType(type, false, out var value, NeoClrCapabilities.Shared)) throw new InvalidOperationException("unsupported native value type");
        return Map(value, resolveClass, resolveExternal);
    }

    public PrimitiveType Map(EmissionPrimitiveType type) => type switch
    {
        EmissionPrimitiveType.String => PrimitiveType.String,
        EmissionPrimitiveType.Int32 => PrimitiveType.Int32,
        EmissionPrimitiveType.Byte => PrimitiveType.Byte,
        EmissionPrimitiveType.Int64 => PrimitiveType.Int64,
        EmissionPrimitiveType.Boolean => PrimitiveType.Boolean,
        EmissionPrimitiveType.NoResult => PrimitiveType.Void,
        _ => throw new ArgumentOutOfRangeException(nameof(type))
    };
}
