namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical body/declaration types. NoResult describes a callable result contract,
// not an inhabited Unit/Void value or a legal parameter/local type.
internal enum EmissionPrimitiveType { NoResult, Int32, Int64, Boolean, String }

internal static class EmissionPrimitiveTypes
{
    internal static bool TryGetValueType(ITypeSymbol symbol, out EmissionPrimitiveType type)
    {
        type = symbol.SpecialType switch
        {
            SpecialType.System_String => EmissionPrimitiveType.String,
            SpecialType.System_Int32 => EmissionPrimitiveType.Int32,
            SpecialType.System_Int64 => EmissionPrimitiveType.Int64,
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
