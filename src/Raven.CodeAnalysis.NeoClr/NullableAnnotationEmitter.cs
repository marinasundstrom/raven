using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Consumes semantic types only; physical signature mapping deliberately erases these facts.
internal static class NullableAnnotationEmitter
{
    internal static NullableAnnotation? Create(ITypeSymbol type)
    {
        var flags = new List<byte>();
        Collect(type, flags);
        return flags.Contains(2) ? new NullableAnnotation(flags) : null;
    }

    private static void Collect(ITypeSymbol type, List<byte> flags)
    {
        if (type is IAddressTypeSymbol address)
        {
            Collect(address.ReferencedType, flags);
            return;
        }
        var physical = type.GetNonNullableType();
        if (physical.IsValueType)
        {
            if (physical is not INamedTypeSymbol { TypeArguments.Length: > 0 } value) return;
            flags.Add(0);
            foreach (var argument in value.TypeArguments) Collect(argument, flags);
            return;
        }
        flags.Add(type.IsNullable ? (byte)2 : (byte)1);
        if (physical is IArrayTypeSymbol array) Collect(array.ElementType, flags);
        else if (physical is INamedTypeSymbol named)
            foreach (var argument in named.TypeArguments) Collect(argument, flags);
    }
}
