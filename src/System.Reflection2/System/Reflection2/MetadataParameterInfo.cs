namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;

/// <summary>
/// Reflection-only <see cref="ParameterInfo"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataParameterInfo : ParameterInfo
{
    private readonly Type _parameterType;
    private readonly string? _name;
    private readonly int _position;
    private readonly ParameterAttributes _attributes;
    private readonly Lazy<IList<CustomAttributeData>>? _customAttributes;
    private readonly Lazy<(bool hasValue, object? value)>? _defaultValue;

    internal MetadataParameterInfo(Type parameterType, string? name, int position, ParameterAttributes attributes, MetadataModule? module = null, ParameterHandle parameterHandle = default, MetadataType? declaringTypeContext = null, IReadOnlyList<Type>? genericMethodArguments = null)
    {
        _parameterType = parameterType;
        _name = name;
        _position = position;
        _attributes = attributes;
        if (module is not null && !parameterHandle.IsNil)
        {
            var parameter = module.Reader.GetParameter(parameterHandle);
            _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(module, parameter.GetCustomAttributes(), declaringTypeContext, genericMethodArguments));
            _defaultValue = new Lazy<(bool hasValue, object? value)>(() => DecodeDefaultValue(module, parameter, parameterType));
        }
    }

    public override Type ParameterType => _parameterType;

    public override string? Name => _name;

    public override int Position => _position;

    public override ParameterAttributes Attributes => _attributes;

    public override object? DefaultValue
    {
        get
        {
            if (_defaultValue is null)
            {
                return DBNull.Value;
            }

            var (hasValue, value) = _defaultValue.Value;
            if (!hasValue)
            {
                return DBNull.Value;
            }

            return value;
        }
    }

    public override object? RawDefaultValue => DefaultValue;

    public override bool HasDefaultValue => _defaultValue?.Value.hasValue ?? false;

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes?.Value ?? Array.Empty<CustomAttributeData>();

    public override object[] GetCustomAttributes(bool inherit)
        => throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
        => throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");

    public override bool IsDefined(Type attributeType, bool inherit)
        => (_customAttributes?.Value ?? Array.Empty<CustomAttributeData>()).Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    private static (bool hasValue, object? value) DecodeDefaultValue(MetadataModule module, Parameter parameter, Type parameterType)
    {
        var constantHandle = parameter.GetDefaultValue();
        if (constantHandle.IsNil)
        {
            return (false, null);
        }

        var reader = module.Reader;
        var constant = reader.GetConstant(constantHandle);
        var blobReader = reader.GetBlobReader(constant.Value);
        var rawValue = blobReader.ReadConstant(constant.TypeCode);

        if (rawValue is null)
        {
            return (true, null);
        }

        var enumType = ResolveRuntimeEnumType(parameterType);
        if (enumType is not null)
        {
            return (true, Enum.ToObject(enumType, rawValue));
        }

        return (true, rawValue);
    }

    private static Type? ResolveRuntimeEnumType(Type parameterType)
    {
        if (parameterType.IsEnum && parameterType.GetType().Assembly == typeof(Type).Assembly)
        {
            return parameterType;
        }

        var assemblyQualifiedName = parameterType.AssemblyQualifiedName ?? parameterType.FullName;
        if (assemblyQualifiedName is null)
        {
            return null;
        }

        var runtimeType = Type.GetType(assemblyQualifiedName, throwOnError: false);
        return runtimeType is not null && runtimeType.IsEnum ? runtimeType : null;
    }
}
