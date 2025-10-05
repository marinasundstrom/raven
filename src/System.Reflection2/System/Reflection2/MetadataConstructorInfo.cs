namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

internal sealed class MetadataConstructorInfo : ConstructorInfo
{
    private readonly MetadataType _declaringType;
    private readonly MetadataReader _reader;
    private readonly MethodDefinitionHandle _handle;
    private readonly MethodDefinition _definition;
    private readonly Lazy<MetadataDecodedSignature> _signature;
    private readonly Lazy<IReadOnlyList<MetadataParameterInfo>> _parameters;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;

    internal MetadataConstructorInfo(MetadataType declaringType, MethodDefinitionHandle handle)
    {
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _reader = declaringType.Reader;
        _handle = handle;
        _definition = _reader.GetMethodDefinition(handle);
        _signature = new Lazy<MetadataDecodedSignature>(DecodeSignature);
        _parameters = new Lazy<IReadOnlyList<MetadataParameterInfo>>(DecodeParameters);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(_declaringType.MetadataModule, _definition.GetCustomAttributes(), _declaringType, null));
    }

    public MethodDefinitionHandle Handle => _handle;

    public override MethodAttributes Attributes => _definition.Attributes;

    public override CallingConventions CallingConvention => (CallingConventions)_signature.Value.Signature.Header.CallingConvention;

    public override int MetadataToken => MetadataTokens.GetToken(_handle);

    public override bool ContainsGenericParameters => _definition.GetGenericParameters().Any();

    public override RuntimeMethodHandle MethodHandle => throw new NotSupportedException();

    public override MethodImplAttributes GetMethodImplementationFlags() => _definition.ImplAttributes;

    public override ParameterInfo[] GetParameters() => _parameters.Value.Cast<ParameterInfo>().ToArray();

    public override MethodBody? GetMethodBody()
    {
        var bodyBlock = _declaringType.MetadataModule.TryGetMethodBody(_definition.RelativeVirtualAddress);
        if (bodyBlock is null)
        {
            return null;
        }

        var declaringArguments = _declaringType.GetGenericArguments();
        return new MetadataMethodBody(
            _declaringType.MetadataModule,
            bodyBlock,
            _declaringType,
            declaringArguments,
            methodTypeArguments: null);
    }

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

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? DeclaringType => _declaringType;

    public override Type? ReflectedType => DeclaringType;

    public override object? Invoke(object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        var result = bridge.Invoke(this, invokeAttr, binder, parameters, culture);
        return result ?? throw new InvalidOperationException("Constructor invocation must return an instance.");
    }

    public override object Invoke(BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        var result = bridge.Invoke(this, invokeAttr, binder, parameters, culture);
        return result ?? throw new InvalidOperationException("Constructor invocation must return an instance.");
    }

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override bool IsGenericMethod => false;

    public override bool IsGenericMethodDefinition => false;

    private MetadataDecodedSignature DecodeSignature()
    {
        var provider = new MetadataSignatureTypeProvider(_declaringType.MetadataModule, _declaringType.GetGenericArguments(), null, _declaringType);
        var signature = _definition.DecodeSignature(provider, _declaringType);
        var (returnModifiers, parameterModifiers) = MetadataSignatureDecoder.DecodeMethodCustomModifiers(
            _declaringType.MetadataModule,
            _definition,
            _declaringType,
            _declaringType.GetGenericArguments(),
            genericMethodParameters: null);

        return new MetadataDecodedSignature(signature, returnModifiers, parameterModifiers);
    }

    private IReadOnlyList<MetadataParameterInfo> DecodeParameters()
    {
        var signatureInfo = _signature.Value;
        var signature = signatureInfo.Signature;
        var parameters = new List<MetadataParameterInfo>();
        var parameterHandles = _definition.GetParameters().ToArray();
        var parameterMap = new Dictionary<int, (ParameterHandle Handle, Parameter Parameter)>();
        foreach (var handle in parameterHandles)
        {
            var parameter = _reader.GetParameter(handle);
            if (parameter.SequenceNumber > 0)
            {
                parameterMap[parameter.SequenceNumber] = (handle, parameter);
            }
        }

        for (var i = 0; i < signature.ParameterTypes.Length; i++)
        {
            var parameterType = signature.ParameterTypes[i];
            string? name = null;
            ParameterAttributes attributes = ParameterAttributes.None;
            ParameterHandle parameterHandle = default;
            if (parameterMap.TryGetValue(i + 1, out var entry))
            {
                name = entry.Parameter.Name.IsNil ? null : _reader.GetString(entry.Parameter.Name);
                attributes = entry.Parameter.Attributes;
                parameterHandle = entry.Handle;
            }

            var modifiers = i < signatureInfo.ParameterCustomModifiers.Length
                ? signatureInfo.ParameterCustomModifiers[i]
                : MetadataCustomModifiers.Empty;

            parameters.Add(new MetadataParameterInfo(parameterType, name, i, attributes, _declaringType.MetadataModule, parameterHandle, _declaringType, null, this, requiredCustomModifiers: modifiers.Required, optionalCustomModifiers: modifiers.Optional));
        }

        return parameters;
    }
}
