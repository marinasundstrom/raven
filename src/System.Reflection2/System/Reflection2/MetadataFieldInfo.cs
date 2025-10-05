namespace System.Reflection2;

using System;
using System.Globalization;
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

    internal MetadataFieldInfo(MetadataType declaringType, FieldDefinitionHandle handle)
    {
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _reader = declaringType.Reader;
        _handle = handle;
        _definition = _reader.GetFieldDefinition(handle);
        _fieldType = new Lazy<Type>(ResolveFieldType);
    }

    public override FieldAttributes Attributes => _definition.Attributes;

    public override RuntimeFieldHandle FieldHandle => throw new NotSupportedException();

    public override Type FieldType => _fieldType.Value;

    public override Type? DeclaringType => _declaringType;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override bool IsDefined(Type attributeType, bool inherit) => false;

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
        => throw new NotSupportedException("Metadata-only fields cannot retrieve values.");

    public override void SetValue(object? obj, object? value, BindingFlags invokeAttr, Binder? binder, CultureInfo? culture)
        => throw new NotSupportedException("Metadata-only fields cannot set values.");

    private Type ResolveFieldType()
    {
        var provider = new MetadataSignatureTypeProvider(_declaringType.Module as MetadataModule ?? throw new InvalidOperationException(), _declaringType.GetGenericArguments());
        var signature = _definition.DecodeSignature(provider, _declaringType);
        return signature;
    }
}
