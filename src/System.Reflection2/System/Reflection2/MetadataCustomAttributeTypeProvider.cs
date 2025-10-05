namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

/// <summary>
/// Provides type resolution for custom attribute decoding.
/// </summary>
internal sealed class MetadataCustomAttributeTypeProvider : ICustomAttributeTypeProvider<Type>
{
    private readonly MetadataModule _module;
    private readonly IReadOnlyList<Type>? _genericTypeParameters;
    private readonly IReadOnlyList<Type>? _genericMethodParameters;
    private Dictionary<int, MetadataSignatureGenericParameterType>? _typeParameterFallbacks;
    private Dictionary<int, MetadataSignatureGenericParameterType>? _methodParameterFallbacks;

    public MetadataCustomAttributeTypeProvider(MetadataModule module, IReadOnlyList<Type>? genericTypeParameters, IReadOnlyList<Type>? genericMethodParameters)
    {
        _module = module ?? throw new ArgumentNullException(nameof(module));
        _genericTypeParameters = genericTypeParameters;
        _genericMethodParameters = genericMethodParameters;
    }

    public Type GetArrayType(Type elementType, ArrayShape shape)
        => _module.GetArrayType(elementType, shape.Rank);

    public Type GetByReferenceType(Type elementType)
        => _module.GetByRefType(elementType);

    public Type GetFunctionPointerType(MethodSignature<Type> signature)
        => _module.GetFunctionPointerType(signature);

    public Type GetGenericInstantiation(Type genericType, ImmutableArray<Type> typeArguments)
        => genericType.MakeGenericType(typeArguments.ToArray());

    public Type GetGenericMethodParameter(int index)
    {
        if (_genericMethodParameters is not null && index < _genericMethodParameters.Count)
        {
            return _genericMethodParameters[index];
        }

        return GetOrCreateFallback(ref _methodParameterFallbacks, index, isMethodParameter: true);
    }

    public Type GetGenericTypeParameter(int index)
    {
        if (_genericTypeParameters is not null && index < _genericTypeParameters.Count)
        {
            return _genericTypeParameters[index];
        }

        return GetOrCreateFallback(ref _typeParameterFallbacks, index, isMethodParameter: false);
    }

    public Type GetModifiedType(Type modifier, Type unmodifiedType, bool isRequired)
        => unmodifiedType;

    public Type GetPinnedType(Type elementType)
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

    public Type GetSystemType()
        => typeof(Type);

    public bool IsSystemType(Type type)
        => type == typeof(Type);

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

    public Type GetTypeFromSerializedName(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
        {
            throw new ArgumentNullException(nameof(name));
        }

        var assemblyNameSeparator = name.IndexOf(',');
        MetadataAssembly assembly;
        string typeName;

        var metadataAssembly = _module.Assembly as MetadataAssembly
            ?? throw new InvalidOperationException("Module is not backed by metadata assembly.");

        if (assemblyNameSeparator < 0)
        {
            assembly = metadataAssembly;
            typeName = name.Trim();
        }
        else
        {
            typeName = name.Substring(0, assemblyNameSeparator).Trim();
            var assemblyDescriptor = name.Substring(assemblyNameSeparator + 1).Trim();
            var simpleName = new AssemblyName(assemblyDescriptor).Name ?? assemblyDescriptor;
            assembly = metadataAssembly.LoadContext.Resolve(simpleName)
                ?? throw new TypeLoadException($"Unable to resolve assembly '{assemblyDescriptor}' for serialized type '{name}'.");
        }

        return assembly.GetType(typeName, throwOnError: true, ignoreCase: false)!;
    }

    public Type GetTypeFromSpecification(MetadataReader reader, MetadataType? genericContext, TypeSpecificationHandle handle, byte rawTypeKind)
    {
        var specification = reader.GetTypeSpecification(handle);
        var provider = new MetadataSignatureTypeProvider(_module, _genericTypeParameters, _genericMethodParameters, genericContext);
        return specification.DecodeSignature(provider, genericContext);
    }

    public PrimitiveTypeCode GetUnderlyingEnumType(Type type)
    {
        if (type is MetadataType metadataType)
        {
            var valueField = metadataType.GetField("value__", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
                ?? throw new NotSupportedException($"Enum type '{metadataType.FullName}' does not define a backing field.");

            return GetPrimitiveTypeCode(valueField.FieldType);
        }

        if (!type.IsEnum)
        {
            throw new ArgumentException("Type is not an enum.", nameof(type));
        }

        return GetPrimitiveTypeCode(Enum.GetUnderlyingType(type));
    }

    private MetadataSignatureGenericParameterType GetOrCreateFallback(ref Dictionary<int, MetadataSignatureGenericParameterType>? cache, int index, bool isMethodParameter)
    {
        cache ??= new Dictionary<int, MetadataSignatureGenericParameterType>();
        if (!cache.TryGetValue(index, out var parameter))
        {
            parameter = new MetadataSignatureGenericParameterType(_module, null, isMethodParameter, index);
            cache[index] = parameter;
        }

        return parameter;
    }

    private static PrimitiveTypeCode GetPrimitiveTypeCode(Type type)
        => Type.GetTypeCode(type) switch
        {
            TypeCode.Boolean => PrimitiveTypeCode.Boolean,
            TypeCode.SByte => PrimitiveTypeCode.SByte,
            TypeCode.Byte => PrimitiveTypeCode.Byte,
            TypeCode.Int16 => PrimitiveTypeCode.Int16,
            TypeCode.UInt16 => PrimitiveTypeCode.UInt16,
            TypeCode.Int32 => PrimitiveTypeCode.Int32,
            TypeCode.UInt32 => PrimitiveTypeCode.UInt32,
            TypeCode.Int64 => PrimitiveTypeCode.Int64,
            TypeCode.UInt64 => PrimitiveTypeCode.UInt64,
            TypeCode.Char => PrimitiveTypeCode.Char,
            _ => throw new NotSupportedException($"Enum underlying type '{type}' is not supported."),
        };
}
