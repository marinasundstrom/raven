using NeoCLR.Metadata.Experimental.Model;

using Raven.CodeAnalysis.CodeGen.Portable;

namespace Raven.CodeAnalysis.NeoClr;

internal sealed class NeoClrTypeMapper : IEmissionTypeMapper<PrimitiveType>
{
    internal static IEmissionTypeMapper<PrimitiveType> Instance { get; } = new NeoClrTypeMapper();

    internal static SignatureType Map(EmissionType type, Func<INamedTypeSymbol, TypeBuilder> resolveClass)
    {
        if (type.OwnerParameter is { } ownerParameter) return SignatureType.TypeParameter(ownerParameter.Ordinal);
        if (type.MethodParameter is { } parameter) return SignatureType.MethodParameter(parameter.Ordinal);
        if (type.Primitive is { } p) return Instance.Map(p);
        if (type.Array is { } array)
        {
            CallableSignature.TryType(array.ElementType, false, out var element);
            return SignatureType.ArrayOf(Map(element, resolveClass));
        }
        return resolveClass(type.Class!);
    }

    internal static SignatureType Map(ITypeSymbol type, Func<INamedTypeSymbol, TypeBuilder> resolveClass)
    {
        if (!CallableSignature.TryType(type, false, out var value)) throw new InvalidOperationException("unsupported native value type");
        return Map(value, resolveClass);
    }

    public PrimitiveType Map(EmissionPrimitiveType type) => type switch
    {
        EmissionPrimitiveType.String => PrimitiveType.String,
        EmissionPrimitiveType.Int32 => PrimitiveType.Int32,
        EmissionPrimitiveType.Int64 => PrimitiveType.Int64,
        EmissionPrimitiveType.Boolean => PrimitiveType.Boolean,
        EmissionPrimitiveType.NoResult => PrimitiveType.Void,
        _ => throw new ArgumentOutOfRangeException(nameof(type))
    };
}
