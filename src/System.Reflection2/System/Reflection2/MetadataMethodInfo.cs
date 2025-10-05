namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Linq;

/// <summary>
/// Reflection-only <see cref="MethodInfo"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataMethodInfo : MethodInfo
{
    private readonly MetadataType _declaringTypeDefinition;
    private readonly Type _declaringTypeInstance;
    private readonly MetadataReader _reader;
    private readonly MethodDefinitionHandle _handle;
    private readonly MethodDefinition _definition;
    private readonly Lazy<MetadataDecodedSignature> _signature;
    private readonly Lazy<IReadOnlyList<MetadataParameterInfo>> _parameters;
    private readonly Lazy<IReadOnlyList<Type>> _genericArguments;
    private readonly Type[]? _instantiatedMethodArguments;
    private readonly MetadataMethodInfo? _genericMethodDefinition;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;
    private readonly Lazy<ParameterMetadata> _parameterMetadata;
    private readonly Lazy<MetadataParameterInfo> _returnParameter;

    internal MetadataMethodInfo(MetadataType declaringTypeDefinition, MethodDefinitionHandle handle)
        : this(declaringTypeDefinition, declaringTypeDefinition, handle, null, null)
    {
    }

    internal MetadataMethodInfo(MetadataType declaringTypeDefinition, Type declaringTypeInstance, MethodDefinitionHandle handle, Type[]? methodTypeArguments, MetadataMethodInfo? genericDefinition)
    {
        _declaringTypeDefinition = declaringTypeDefinition ?? throw new ArgumentNullException(nameof(declaringTypeDefinition));
        _declaringTypeInstance = declaringTypeInstance ?? throw new ArgumentNullException(nameof(declaringTypeInstance));
        _reader = declaringTypeDefinition.Reader;
        _handle = handle;
        _definition = _reader.GetMethodDefinition(handle);
        _instantiatedMethodArguments = methodTypeArguments;
        _genericMethodDefinition = genericDefinition;
        _signature = new Lazy<MetadataDecodedSignature>(DecodeSignature);
        _parameters = new Lazy<IReadOnlyList<MetadataParameterInfo>>(DecodeParameters);
        _genericArguments = new Lazy<IReadOnlyList<Type>>(ResolveGenericArguments);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(_declaringTypeDefinition.MetadataModule, _definition.GetCustomAttributes(), _declaringTypeDefinition, GetGenericArguments()));
        _parameterMetadata = new Lazy<ParameterMetadata>(LoadParameterMetadata);
        _returnParameter = new Lazy<MetadataParameterInfo>(CreateReturnParameter);
    }

    public MethodDefinitionHandle Handle => _handle;

    public override MethodAttributes Attributes => _definition.Attributes;

    public override CallingConventions CallingConvention => (CallingConventions)_signature.Value.Signature.Header.CallingConvention;

    public override int MetadataToken => MetadataTokens.GetToken(_handle);

    public override bool ContainsGenericParameters
    {
        get
        {
            if (_instantiatedMethodArguments is null)
            {
                return _definition.GetGenericParameters().Any() || _declaringTypeInstance.ContainsGenericParameters;
            }

            return _instantiatedMethodArguments.Any(t => t.ContainsGenericParameters) || _declaringTypeInstance.ContainsGenericParameters;
        }
    }

    public override RuntimeMethodHandle MethodHandle => throw new NotSupportedException();

    public override MethodImplAttributes GetMethodImplementationFlags() => _definition.ImplAttributes;

    public override ParameterInfo ReturnParameter => _returnParameter.Value;

    public override Type ReturnType => _signature.Value.Signature.ReturnType;

    public override ICustomAttributeProvider ReturnTypeCustomAttributes => this;

    public override Type? DeclaringType => _declaringTypeInstance;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
    {
        var bridge = _declaringTypeDefinition.MetadataModule.RuntimeBridge;
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

        var bridge = _declaringTypeDefinition.MetadataModule.RuntimeBridge;
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

        var bridge = _declaringTypeDefinition.MetadataModule.RuntimeBridge;
        if (bridge is not null)
        {
            return bridge.IsDefined(this, attributeType, inherit);
        }

        return _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    }

    public override MethodInfo GetBaseDefinition()
    {
        if (IsConstructorDefinition)
        {
            return this;
        }

        if (!IsVirtual)
        {
            return this;
        }

        if ((_definition.Attributes & MethodAttributes.NewSlot) == MethodAttributes.NewSlot)
        {
            return this;
        }

        var baseType = _declaringTypeInstance.BaseType;
        const BindingFlags searchFlags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly;

        while (baseType is not null)
        {
            foreach (var candidate in baseType.GetMethods(searchFlags))
            {
                if (!candidate.IsVirtual)
                {
                    continue;
                }

                if (!MatchesOverrideSignature(candidate))
                {
                    continue;
                }

                return candidate.GetBaseDefinition();
            }

            baseType = baseType.BaseType;
        }

        return this;
    }

    private bool MatchesOverrideSignature(MethodInfo candidate)
    {
        if (!string.Equals(candidate.Name, Name, StringComparison.Ordinal))
        {
            return false;
        }

        if (IsGenericMethod != candidate.IsGenericMethod)
        {
            return false;
        }

        if (IsGenericMethod && GetGenericArguments().Length != candidate.GetGenericArguments().Length)
        {
            return false;
        }

        if (!Equals(candidate.ReturnType, ReturnType))
        {
            return false;
        }

        var parameters = GetParameters();
        var candidateParameters = candidate.GetParameters();
        if (parameters.Length != candidateParameters.Length)
        {
            return false;
        }

        for (var i = 0; i < parameters.Length; i++)
        {
            if (!Equals(parameters[i].ParameterType, candidateParameters[i].ParameterType))
            {
                return false;
            }
        }

        return true;
    }

    public override MethodInfo MakeGenericMethod(params Type[] typeArguments)
    {
        if (typeArguments is null)
        {
            throw new ArgumentNullException(nameof(typeArguments));
        }

        if (!IsGenericMethodDefinition)
        {
            throw new InvalidOperationException($"Method '{Name}' is not a generic method definition.");
        }

        var parameters = _definition.GetGenericParameters();
        if (parameters.Count != typeArguments.Length)
        {
            throw new ArgumentException($"Method '{Name}' expects {parameters.Count} type arguments.", nameof(typeArguments));
        }

        return new MetadataMethodInfo(_declaringTypeDefinition, _declaringTypeInstance, _handle, typeArguments.ToArray(), this);
    }

    public override MethodInfo GetGenericMethodDefinition()
    {
        if (IsGenericMethodDefinition)
        {
            return this;
        }

        if (_genericMethodDefinition is null)
        {
            throw new InvalidOperationException("Method is not a constructed generic method.");
        }

        return _genericMethodDefinition;
    }

    public override Type[] GetGenericArguments() => _genericArguments.Value.ToArray();

    public override bool IsGenericMethod => _definition.GetGenericParameters().Any();

    public override bool IsGenericMethodDefinition => _definition.GetGenericParameters().Any() && _instantiatedMethodArguments is null;

    public override ParameterInfo[] GetParameters() => _parameters.Value.Cast<ParameterInfo>().ToArray();

    public override MethodBody? GetMethodBody()
    {
        var bodyBlock = _declaringTypeDefinition.MetadataModule.TryGetMethodBody(_definition.RelativeVirtualAddress);
        if (bodyBlock is null)
        {
            return null;
        }

        var declaringArguments = _declaringTypeInstance.GetGenericArguments();
        var methodArguments = _instantiatedMethodArguments ?? _genericArguments.Value;
        return new MetadataMethodBody(
            _declaringTypeDefinition.MetadataModule,
            bodyBlock,
            _declaringTypeDefinition,
            declaringArguments,
            methodArguments);
    }

    public override object? Invoke(object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
    {
        var bridge = _declaringTypeDefinition.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        return bridge.Invoke(this, obj, invokeAttr, binder, parameters, culture);
    }

    internal bool IsConstructorDefinition => Name is ".ctor" or ".cctor";

    private MetadataDecodedSignature DecodeSignature()
    {
        var methodGenericArguments = _instantiatedMethodArguments ?? _genericArguments.Value;
        var provider = new MetadataSignatureTypeProvider(_declaringTypeDefinition.MetadataModule, _declaringTypeInstance.GetGenericArguments(), methodGenericArguments, _declaringTypeDefinition);
        var signature = _definition.DecodeSignature(provider, _declaringTypeDefinition);
        var (returnModifiers, parameterModifiers) = MetadataSignatureDecoder.DecodeMethodCustomModifiers(
            _declaringTypeDefinition.MetadataModule,
            _definition,
            _declaringTypeDefinition,
            _declaringTypeInstance.GetGenericArguments(),
            methodGenericArguments);

        return new MetadataDecodedSignature(signature, returnModifiers, parameterModifiers);
    }

    private IReadOnlyList<MetadataParameterInfo> DecodeParameters()
    {
        var signatureInfo = _signature.Value;
        var signature = signatureInfo.Signature;
        var parameters = new List<MetadataParameterInfo>();
        var parameterMetadata = _parameterMetadata.Value;

        for (var i = 0; i < signature.ParameterTypes.Length; i++)
        {
            var parameterType = signature.ParameterTypes[i];
            string? name = null;
            ParameterAttributes attributes = ParameterAttributes.None;
            ParameterHandle parameterHandle = default;
            if (parameterMetadata.Parameters.TryGetValue(i + 1, out var entry))
            {
                name = entry.Parameter.Name.IsNil ? null : _reader.GetString(entry.Parameter.Name);
                attributes = entry.Parameter.Attributes;
                parameterHandle = entry.Handle;
            }

            var modifiers = i < signatureInfo.ParameterCustomModifiers.Length
                ? signatureInfo.ParameterCustomModifiers[i]
                : MetadataCustomModifiers.Empty;

            parameters.Add(new MetadataParameterInfo(parameterType, name, i, attributes, _declaringTypeDefinition.MetadataModule, parameterHandle, _declaringTypeDefinition, GetGenericArguments(), this, requiredCustomModifiers: modifiers.Required, optionalCustomModifiers: modifiers.Optional));
        }

        return parameters;
    }

    private MetadataParameterInfo CreateReturnParameter()
    {
        var signatureInfo = _signature.Value;
        var parameterMetadata = _parameterMetadata.Value;
        ParameterAttributes attributes = ParameterAttributes.None;
        ParameterHandle handle = default;
        if (parameterMetadata.ReturnParameter is { } metadata)
        {
            attributes = metadata.Parameter.Attributes;
            handle = metadata.Handle;
        }

        return new MetadataParameterInfo(
            signatureInfo.Signature.ReturnType,
            name: null,
            position: -1,
            attributes,
            _declaringTypeDefinition.MetadataModule,
            handle,
            _declaringTypeDefinition,
            GetGenericArguments(),
            this,
            isReturnParameter: true,
            requiredCustomModifiers: signatureInfo.ReturnCustomModifiers.Required,
            optionalCustomModifiers: signatureInfo.ReturnCustomModifiers.Optional);
    }

    private ParameterMetadata LoadParameterMetadata()
    {
        var returnParameter = default((ParameterHandle Handle, Parameter Parameter)?);
        var parameters = new Dictionary<int, (ParameterHandle Handle, Parameter Parameter)>();
        foreach (var handle in _definition.GetParameters())
        {
            var parameter = _reader.GetParameter(handle);
            if (parameter.SequenceNumber == 0)
            {
                returnParameter = (handle, parameter);
            }
            else
            {
                parameters[parameter.SequenceNumber] = (handle, parameter);
            }
        }

        return new ParameterMetadata(returnParameter, parameters);
    }

    private IReadOnlyList<Type> ResolveGenericArguments()
    {
        if (_instantiatedMethodArguments is not null)
        {
            return _instantiatedMethodArguments;
        }

        var handles = _definition.GetGenericParameters().ToArray();
        if (handles.Length == 0)
        {
            return Array.Empty<Type>();
        }

        var result = new Type[handles.Length];
        for (var i = 0; i < handles.Length; i++)
        {
            result[i] = new MetadataGenericParameterType(_declaringTypeDefinition.MetadataModule, _declaringTypeDefinition, this, handles[i]);
        }

        return result;
    }

    private readonly struct ParameterMetadata
    {
        public ParameterMetadata((ParameterHandle Handle, Parameter Parameter)? returnParameter, Dictionary<int, (ParameterHandle Handle, Parameter Parameter)> parameters)
        {
            ReturnParameter = returnParameter;
            Parameters = parameters;
        }

        public (ParameterHandle Handle, Parameter Parameter)? ReturnParameter { get; }

        public Dictionary<int, (ParameterHandle Handle, Parameter Parameter)> Parameters { get; }
    }
}
