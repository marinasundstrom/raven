namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

/// <summary>
/// Reflection-only <see cref="FieldInfo"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataFieldInfo : FieldInfo
{
    private readonly MetadataType _declaringType;
    private readonly MetadataReader _reader;
    private readonly FieldDefinitionHandle _handle;
    private readonly FieldDefinition _definition;
    private readonly Lazy<Type> _fieldType;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;

    internal MetadataFieldInfo(MetadataType declaringType, FieldDefinitionHandle handle)
    {
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _reader = declaringType.Reader;
        _handle = handle;
        _definition = _reader.GetFieldDefinition(handle);
        _fieldType = new Lazy<Type>(ResolveFieldType);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(_declaringType.MetadataModule, _definition.GetCustomAttributes(), _declaringType, null));
    }

    internal FieldDefinitionHandle Handle => _handle;

    public override FieldAttributes Attributes => _definition.Attributes;

    public override RuntimeFieldHandle FieldHandle => throw new NotSupportedException();

    public override Type FieldType => _fieldType.Value;

    public override Type? DeclaringType => _declaringType;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override int MetadataToken => MetadataTokens.GetToken(_handle);

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");
        }

        return bridge.GetCustomAttributes(this, attributeType: null, inherit);
    }

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
    {
        if (attributeType is null)
        {
            throw new ArgumentNullException(nameof(attributeType));
        }

        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");
        }

        return bridge.GetCustomAttributes(this, attributeType, inherit);
    }

    public override bool IsDefined(Type attributeType, bool inherit)
    {
        if (attributeType is null)
        {
            throw new ArgumentNullException(nameof(attributeType));
        }

        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is not null)
        {
            return bridge.IsDefined(this, attributeType, inherit);
        }

        return _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    }

    public override object? GetRawConstantValue()
    {
        var defaultValueHandle = _definition.GetDefaultValue();
        if (!defaultValueHandle.IsNil)
        {
            var constant = _reader.GetConstant(defaultValueHandle);
            var blobReader = _reader.GetBlobReader(constant.Value);
            var rawValue = blobReader.ReadConstant(constant.TypeCode);

            if (rawValue is null)
            {
                return null;
            }

            var fieldType = FieldType;

            if (MetadataConstantDecoder.IsEnumLike(fieldType))
            {
                var runtimeEnumType = MetadataConstantDecoder.TryResolveRuntimeEnumType(fieldType);
                if (runtimeEnumType is not null)
                {
                    return Enum.ToObject(runtimeEnumType, rawValue);
                }
            }
            else if (fieldType == typeof(decimal) && rawValue is double doubleValue)
            {
                return Convert.ToDecimal(doubleValue);
            }

            return rawValue;
        }

        var attributes = _customAttributes.Value;
        if (MetadataConstantDecoder.TryDecodeDecimalConstant(attributes, out var decimalValue))
        {
            return decimalValue;
        }

        if (MetadataConstantDecoder.TryDecodeDateTimeConstant(attributes, out var dateTimeValue))
        {
            return dateTimeValue;
        }

        throw new InvalidOperationException($"Field '{Name}' does not have a default value.");
    }

    public override object? GetValue(object? obj)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Metadata-only fields cannot retrieve values. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        return bridge.GetValue(this, obj);
    }

    public override void SetValue(object? obj, object? value, BindingFlags invokeAttr, Binder? binder, CultureInfo? culture)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Metadata-only fields cannot set values. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        bridge.SetValue(this, obj, value, invokeAttr, binder, culture);
    }

    private Type ResolveFieldType()
    {
        var provider = new MetadataSignatureTypeProvider(_declaringType.Module as MetadataModule ?? throw new InvalidOperationException(), _declaringType.GetGenericArguments(), null, _declaringType);
        var signature = _definition.DecodeSignature(provider, _declaringType);
        return signature;
    }
}
