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

    public override FieldAttributes Attributes => _definition.Attributes;

    public override RuntimeFieldHandle FieldHandle => throw new NotSupportedException();

    public override Type FieldType => _fieldType.Value;

    public override Type? DeclaringType => _declaringType;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
        => throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
        => throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");

    public override bool IsDefined(Type attributeType, bool inherit)
        => _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));

    public override object? GetRawConstantValue()
    {
        if (_definition.GetDefaultValue().IsNil)
        {
            return null;
        }

        var constant = _reader.GetConstant(_definition.GetDefaultValue());
        return constant.Value;
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
