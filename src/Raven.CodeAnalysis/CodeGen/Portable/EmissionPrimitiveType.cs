namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical body/declaration types. NoResult describes a callable result contract,
// not an inhabited Unit/Void value or a legal parameter/local type.
internal enum EmissionPrimitiveType { NoResult, Int32, Int64, Boolean, String, Byte, Single, Double, SByte, Int16, UInt16, UInt32, UInt64, IntPtr, UIntPtr }

internal static class EmissionPrimitiveTypes
{
    internal static bool TryGetValueType(ITypeSymbol symbol, out EmissionPrimitiveType type)
    {
        type = symbol.SpecialType switch
        {
            SpecialType.System_Byte => EmissionPrimitiveType.Byte,
            SpecialType.System_SByte => EmissionPrimitiveType.SByte,
            SpecialType.System_Int16 => EmissionPrimitiveType.Int16,
            SpecialType.System_UInt16 => EmissionPrimitiveType.UInt16,
            SpecialType.System_UInt32 => EmissionPrimitiveType.UInt32,
            SpecialType.System_UInt64 => EmissionPrimitiveType.UInt64,
            SpecialType.System_IntPtr => EmissionPrimitiveType.IntPtr,
            SpecialType.System_UIntPtr => EmissionPrimitiveType.UIntPtr,

            SpecialType.System_String => EmissionPrimitiveType.String,
            SpecialType.System_Int32 => EmissionPrimitiveType.Int32,
            SpecialType.System_Int64 => EmissionPrimitiveType.Int64,
            SpecialType.System_Single => EmissionPrimitiveType.Single,
            SpecialType.System_Double => EmissionPrimitiveType.Double,
            SpecialType.System_Boolean => EmissionPrimitiveType.Boolean,
            _ => EmissionPrimitiveType.NoResult
        };
        return type != EmissionPrimitiveType.NoResult;
    }

    internal static bool TryGetReturnType(ITypeSymbol symbol, out EmissionPrimitiveType type)
    {
        if (symbol.SpecialType is SpecialType.System_Unit or SpecialType.System_Void)
        {
            type = EmissionPrimitiveType.NoResult;
            return true;
        }
        return TryGetValueType(symbol, out type);
    }
}

// Concrete target handles remain in their backend, including selected-core CLR
// types and the independent native metadata library's primitive identities.
internal interface IEmissionTypeMapper<TType>
{
    TType Map(EmissionPrimitiveType type);
}
