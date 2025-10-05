namespace System.Reflection2;

using System;
using System.Collections.Immutable;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Linq;

internal sealed class MetadataSignatureTypeProvider : ISignatureTypeProvider<Type, MetadataType?>
{
    private readonly MetadataModule _module;
    private readonly IReadOnlyList<Type>? _genericTypeParameters;
    private readonly IReadOnlyList<Type>? _genericMethodParameters;

    public MetadataSignatureTypeProvider(MetadataModule module, IReadOnlyList<Type>? genericTypeParameters = null, IReadOnlyList<Type>? genericMethodParameters = null)
    {
        _module = module;
        _genericTypeParameters = genericTypeParameters;
        _genericMethodParameters = genericMethodParameters;
    }

    public Type GetArrayType(Type elementType, ArrayShape shape)
        => elementType.MakeArrayType(shape.Rank);

    public Type GetByReferenceType(Type elementType)
        => elementType.MakeByRefType();

    public Type GetFunctionPointerType(MethodSignature<Type> signature)
        => throw new NotSupportedException("Function pointer signatures are not supported in metadata-only context.");

    public Type GetGenericInstantiation(Type genericType, ImmutableArray<Type> typeArguments)
        => genericType.MakeGenericType(typeArguments.ToArray());

    public Type GetGenericMethodParameter(MetadataType? genericContext, int index)
    {
        if (_genericMethodParameters is not null && index < _genericMethodParameters.Count)
        {
            return _genericMethodParameters[index];
        }

        throw new NotSupportedException("Generic method parameters are not supported in this context.");
    }

    public Type GetGenericTypeParameter(MetadataType? genericContext, int index)
    {
        if (_genericTypeParameters is not null && index < _genericTypeParameters.Count)
        {
            return _genericTypeParameters[index];
        }

        throw new NotSupportedException("Generic type parameters are not supported in this context.");
    }

    public Type GetModifiedType(Type modifier, Type unmodifiedType, bool isRequired)
        => unmodifiedType;

    public Type GetPinnedType(Type elementType)
        => elementType;

    public Type GetPointerType(Type elementType)
        => elementType.MakePointerType();

    public Type GetPrimitiveType(PrimitiveTypeCode typeCode)
        => typeCode switch
        {
            PrimitiveTypeCode.Boolean => typeof(bool),
            PrimitiveTypeCode.Byte => typeof(byte),
            PrimitiveTypeCode.SByte => typeof(sbyte),
            PrimitiveTypeCode.Char => typeof(char),
            PrimitiveTypeCode.Int16 => typeof(short),
            PrimitiveTypeCode.UInt16 => typeof(ushort),
            PrimitiveTypeCode.Int32 => typeof(int),
            PrimitiveTypeCode.UInt32 => typeof(uint),
            PrimitiveTypeCode.Int64 => typeof(long),
            PrimitiveTypeCode.UInt64 => typeof(ulong),
            PrimitiveTypeCode.Single => typeof(float),
            PrimitiveTypeCode.Double => typeof(double),
            PrimitiveTypeCode.IntPtr => typeof(IntPtr),
            PrimitiveTypeCode.UIntPtr => typeof(UIntPtr),
            PrimitiveTypeCode.Object => typeof(object),
            PrimitiveTypeCode.String => typeof(string),
            PrimitiveTypeCode.Void => typeof(void),
            _ => throw new NotSupportedException($"Primitive type '{typeCode}' is not supported."),
        };

    public Type GetSZArrayType(Type elementType)
        => elementType.MakeArrayType();

    public Type GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        => _module.ResolveType(handle);

    public Type GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
        => _module.ResolveType(handle);

    public Type GetTypeFromSpecification(MetadataReader reader, MetadataType? genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
    {
        var specification = reader.GetTypeSpecification(handle);
        var provider = new MetadataSignatureTypeProvider(_module, _genericTypeParameters, _genericMethodParameters);
        return specification.DecodeSignature(provider, genericContext);
    }
}
