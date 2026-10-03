namespace Raven.CodeAnalysis.CodeGen.Portable;

internal sealed class ReflectionEmitTypeMapper(Func<SpecialType, Type> resolveType) : IEmissionTypeMapper<Type>
{
    public Type Map(EmissionPrimitiveType type) => resolveType(type switch
    {
        EmissionPrimitiveType.String => SpecialType.System_String,
        EmissionPrimitiveType.Int32 => SpecialType.System_Int32,
        EmissionPrimitiveType.Byte => SpecialType.System_Byte,
        EmissionPrimitiveType.Int64 => SpecialType.System_Int64,
        EmissionPrimitiveType.Boolean => SpecialType.System_Boolean,
        EmissionPrimitiveType.NoResult => SpecialType.System_Void,
        _ => throw new ArgumentOutOfRangeException(nameof(type))
    });
}
