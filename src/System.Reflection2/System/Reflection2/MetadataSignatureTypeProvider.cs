namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

internal class MetadataSignatureTypeProvider : ISignatureTypeProvider<Type, MetadataType?>
{
    private readonly MetadataModule _module;
    private readonly IReadOnlyList<Type>? _genericTypeParameters;
    private readonly IReadOnlyList<Type>? _genericMethodParameters;
    private readonly MetadataType? _declaringTypeContext;
    private Dictionary<int, MetadataSignatureGenericParameterType>? _typeParameterFallbacks;
    private Dictionary<int, MetadataSignatureGenericParameterType>? _methodParameterFallbacks;

    public MetadataSignatureTypeProvider(
        MetadataModule module,
        IReadOnlyList<Type>? genericTypeParameters = null,
        IReadOnlyList<Type>? genericMethodParameters = null,
        MetadataType? declaringTypeContext = null)
    {
        _module = module;
        _genericTypeParameters = genericTypeParameters;
        _genericMethodParameters = genericMethodParameters;
        _declaringTypeContext = declaringTypeContext;
    }

    public Type GetArrayType(Type elementType, ArrayShape shape)
        => _module.GetArrayType(elementType, shape.Rank);

    public Type GetByReferenceType(Type elementType)
        => _module.GetByRefType(elementType);

    public Type GetFunctionPointerType(MethodSignature<Type> signature)
        => _module.GetFunctionPointerType(signature);

    public Type GetGenericInstantiation(Type genericType, ImmutableArray<Type> typeArguments)
        => genericType.MakeGenericType(typeArguments.ToArray());

    public Type GetGenericMethodParameter(MetadataType? genericContext, int index)
    {
        if (_genericMethodParameters is not null && index < _genericMethodParameters.Count)
        {
            return _genericMethodParameters[index];
        }

        return GetOrCreateFallbackParameter(ref _methodParameterFallbacks, index, isMethodParameter: true);
    }

    public Type GetGenericTypeParameter(MetadataType? genericContext, int index)
    {
        if (_genericTypeParameters is not null && index < _genericTypeParameters.Count)
        {
            return _genericTypeParameters[index];
        }

        return GetOrCreateFallbackParameter(ref _typeParameterFallbacks, index, isMethodParameter: false);
    }

    public virtual Type GetModifiedType(Type modifier, Type unmodifiedType, bool isRequired)
        => unmodifiedType;

    public virtual Type GetPinnedType(Type elementType)
        => elementType;

    public Type GetPointerType(Type elementType)
        => _module.GetPointerType(elementType);

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
        => _module.GetArrayType(elementType, 1);

    public Type GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind)
        => _module.ResolveType(handle, null);

    public Type GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind)
    {
        try
        {
            return _module.ResolveType(handle, null);
        }
        catch (TypeLoadException)
        {
            var reference = reader.GetTypeReference(handle);
            var @namespace = reference.Namespace.IsNil ? null : reader.GetString(reference.Namespace);
            var name = reference.Name.IsNil ? string.Empty : reader.GetString(reference.Name);
            return new MetadataReferenceType(_module, @namespace, name);
        }
    }

    public Type GetTypeFromSpecification(MetadataReader reader, MetadataType? genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
    {
        var specification = reader.GetTypeSpecification(handle);
        var provider = new MetadataSignatureTypeProvider(_module, _genericTypeParameters, _genericMethodParameters, _declaringTypeContext);
        return specification.DecodeSignature(provider, genericContext);
    }

    private MetadataSignatureGenericParameterType GetOrCreateFallbackParameter(ref Dictionary<int, MetadataSignatureGenericParameterType>? cache, int index, bool isMethodParameter)
    {
        cache ??= new Dictionary<int, MetadataSignatureGenericParameterType>();
        if (!cache.TryGetValue(index, out var placeholder))
        {
            placeholder = new MetadataSignatureGenericParameterType(_module, _declaringTypeContext, isMethodParameter, index);
            cache[index] = placeholder;
        }

        return placeholder;
    }
}

internal sealed class MetadataCustomModifierRecordingTypeProvider : MetadataSignatureTypeProvider
{
    private bool _captureActive;
    private List<Type>? _required;
    private List<Type>? _optional;

    public MetadataCustomModifierRecordingTypeProvider(
        MetadataModule module,
        IReadOnlyList<Type>? genericTypeParameters = null,
        IReadOnlyList<Type>? genericMethodParameters = null,
        MetadataType? declaringTypeContext = null)
        : base(module, genericTypeParameters, genericMethodParameters, declaringTypeContext)
    {
    }

    public void BeginCapture()
    {
        if (_captureActive)
        {
            throw new InvalidOperationException("A custom modifier capture is already active.");
        }

        _captureActive = true;
        _required = null;
        _optional = null;
    }

    public MetadataCustomModifiers EndCapture()
    {
        if (!_captureActive)
        {
            throw new InvalidOperationException("No active custom modifier capture to complete.");
        }

        _captureActive = false;
        var required = _required is null ? Type.EmptyTypes : _required.ToArray();
        var optional = _optional is null ? Type.EmptyTypes : _optional.ToArray();
        _required = null;
        _optional = null;
        return required.Length == 0 && optional.Length == 0
            ? MetadataCustomModifiers.Empty
            : new MetadataCustomModifiers(required, optional);
    }

    public override Type GetModifiedType(Type modifier, Type unmodifiedType, bool isRequired)
    {
        if (_captureActive)
        {
            if (isRequired)
            {
                (_required ??= new List<Type>()).Add(modifier);
            }
            else
            {
                (_optional ??= new List<Type>()).Add(modifier);
            }
        }

        return base.GetModifiedType(modifier, unmodifiedType, isRequired);
    }
}
