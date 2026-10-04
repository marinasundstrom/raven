namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitTypeMapper(Func<SpecialType, Type> resolveType) : IEmissionTypeMapper<Type>
{
    public Type Map(EmissionPrimitiveType type) => resolveType(type switch
    {
        EmissionPrimitiveType.String => SpecialType.System_String,
        EmissionPrimitiveType.Int32 => SpecialType.System_Int32,
        EmissionPrimitiveType.Byte => SpecialType.System_Byte,
        EmissionPrimitiveType.SByte => SpecialType.System_SByte,
        EmissionPrimitiveType.Int16 => SpecialType.System_Int16,
        EmissionPrimitiveType.UInt16 => SpecialType.System_UInt16,
        EmissionPrimitiveType.UInt32 => SpecialType.System_UInt32,
        EmissionPrimitiveType.UInt64 => SpecialType.System_UInt64,
        EmissionPrimitiveType.Int64 => SpecialType.System_Int64,
        EmissionPrimitiveType.Single => SpecialType.System_Single,
        EmissionPrimitiveType.Double => SpecialType.System_Double,
        EmissionPrimitiveType.Boolean => SpecialType.System_Boolean,
        EmissionPrimitiveType.NoResult => SpecialType.System_Void,
        _ => throw new ArgumentOutOfRangeException(nameof(type))
    });
}
