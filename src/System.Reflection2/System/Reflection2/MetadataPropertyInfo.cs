namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Linq;

/// <summary>
/// Reflection-only <see cref="PropertyInfo"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataPropertyInfo : PropertyInfo
{
    private readonly MetadataType _declaringType;
    private readonly PropertyDefinitionHandle _handle;
    private readonly PropertyDefinition _definition;
    private readonly Lazy<Type> _propertyType;
    private readonly Lazy<ParameterInfo[]> _indexParameters;
    private readonly Lazy<MethodInfo?> _getter;
    private readonly Lazy<MethodInfo?> _setter;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;

    internal MetadataPropertyInfo(MetadataType declaringType, PropertyDefinitionHandle handle)
    {
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _handle = handle;
        _definition = declaringType.Reader.GetPropertyDefinition(handle);
        _propertyType = new Lazy<Type>(ResolvePropertyType);
        _indexParameters = new Lazy<ParameterInfo[]>(ResolveIndexParameters);
        _getter = new Lazy<MethodInfo?>(() => ResolveAccessor(_definition.GetAccessors().Getter));
        _setter = new Lazy<MethodInfo?>(() => ResolveAccessor(_definition.GetAccessors().Setter));
        _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(_declaringType.MetadataModule, _definition.GetCustomAttributes(), _declaringType, null));
    }

    public override PropertyAttributes Attributes => _definition.Attributes;

    public override bool CanRead => _getter.Value is not null;

    public override bool CanWrite => _setter.Value is not null;

    public override Type? DeclaringType => _declaringType;

    public override string Name => _declaringType.Reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override MethodInfo? GetMethod => _getter.Value;

    public override MethodInfo? SetMethod => _setter.Value;

    public override Type PropertyType => _propertyType.Value;

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

    public override MethodInfo[] GetAccessors(bool nonPublic)
    {
        var list = new List<MethodInfo>();
        if (_getter.Value is { } getter && (nonPublic || getter.IsPublic))
        {
            list.Add(getter);
        }

        if (_setter.Value is { } setter && (nonPublic || setter.IsPublic))
        {
            list.Add(setter);
        }

        return list.ToArray();
    }

    public override ParameterInfo[] GetIndexParameters()
        => (ParameterInfo[])_indexParameters.Value.Clone();

    public override object? GetValue(object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? index, CultureInfo? culture)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        return bridge.GetValue(this, obj, invokeAttr, binder, index, culture);
    }

    public override void SetValue(object? obj, object? value, BindingFlags invokeAttr, Binder? binder, object?[]? index, CultureInfo? culture)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        bridge.SetValue(this, obj, value, invokeAttr, binder, index, culture);
    }

    public override Type[] GetOptionalCustomModifiers() => Type.EmptyTypes;

    public override Type[] GetRequiredCustomModifiers() => Type.EmptyTypes;

    public override MethodInfo? GetGetMethod(bool nonPublic)
    {
        var getter = _getter.Value;
        if (getter is null)
        {
            return null;
        }

        return getter.IsPublic || nonPublic ? getter : null;
    }

    public override MethodInfo? GetSetMethod(bool nonPublic)
    {
        var setter = _setter.Value;
        if (setter is null)
        {
            return null;
        }

        return setter.IsPublic || nonPublic ? setter : null;
    }

    private Type ResolvePropertyType()
    {
        if (_getter.Value is { } getter)
        {
            return getter.ReturnType;
        }

        if (_setter.Value is { } setter)
        {
            var parameters = setter.GetParameters();
            if (parameters.Length == 0)
            {
                throw new InvalidOperationException("Setter is missing value parameter.");
            }

            return parameters[^1].ParameterType;
        }

        throw new InvalidOperationException("Property does not define any accessors.");
    }

    private ParameterInfo[] ResolveIndexParameters()
    {
        if (_getter.Value is { } getter)
        {
            return getter.GetParameters();
        }

        if (_setter.Value is { } setter)
        {
            var parameters = setter.GetParameters();
            if (parameters.Length <= 1)
            {
                return Array.Empty<ParameterInfo>();
            }

            var result = new ParameterInfo[parameters.Length - 1];
            Array.Copy(parameters, result, result.Length);
            return result;
        }

        return Array.Empty<ParameterInfo>();
    }

    private MethodInfo? ResolveAccessor(MethodDefinitionHandle handle)
        => handle.IsNil ? null : _declaringType.ResolveMethod(handle);
}
