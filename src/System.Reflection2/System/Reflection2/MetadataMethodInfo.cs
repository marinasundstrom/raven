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
    private readonly Lazy<MethodSignature<Type>> _signature;
    private readonly Lazy<IReadOnlyList<MetadataParameterInfo>> _parameters;
    private readonly Lazy<IReadOnlyList<Type>> _genericArguments;
    private readonly Type[]? _instantiatedMethodArguments;
    private readonly MetadataMethodInfo? _genericMethodDefinition;

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
        _signature = new Lazy<MethodSignature<Type>>(DecodeSignature);
        _parameters = new Lazy<IReadOnlyList<MetadataParameterInfo>>(DecodeParameters);
        _genericArguments = new Lazy<IReadOnlyList<Type>>(ResolveGenericArguments);
    }

    public MethodDefinitionHandle Handle => _handle;

    public override MethodAttributes Attributes => _definition.Attributes;

    public override CallingConventions CallingConvention => (CallingConventions)_signature.Value.Header.CallingConvention;

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

    public override ParameterInfo ReturnParameter => new MetadataParameterInfo(_signature.Value.ReturnType, null, -1, ParameterAttributes.None);

    public override Type ReturnType => _signature.Value.ReturnType;

    public override ICustomAttributeProvider ReturnTypeCustomAttributes => this;

    public override Type? DeclaringType => _declaringTypeInstance;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override MethodInfo GetBaseDefinition() => this;

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override bool IsDefined(Type attributeType, bool inherit) => false;

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

    public override object? Invoke(object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
        => throw new NotSupportedException("Invocation is not supported in metadata-only context.");

    internal bool IsConstructorDefinition => Name is ".ctor" or ".cctor";

    private MethodSignature<Type> DecodeSignature()
    {
        var provider = new MetadataSignatureTypeProvider(_declaringTypeDefinition.MetadataModule, _declaringTypeInstance.GetGenericArguments(), GetGenericArguments());
        return _definition.DecodeSignature(provider, _declaringTypeDefinition);
    }

    private IReadOnlyList<MetadataParameterInfo> DecodeParameters()
    {
        var signature = _signature.Value;
        var parameters = new List<MetadataParameterInfo>();
        var parameterHandles = _definition.GetParameters().ToArray();
        for (var i = 0; i < signature.ParameterTypes.Length; i++)
        {
            var parameterType = signature.ParameterTypes[i];
            string? name = null;
            ParameterAttributes attributes = ParameterAttributes.None;
            if (i < parameterHandles.Length)
            {
                var parameter = _reader.GetParameter(parameterHandles[i]);
                name = parameter.Name.IsNil ? null : _reader.GetString(parameter.Name);
                attributes = parameter.Attributes;
            }

            parameters.Add(new MetadataParameterInfo(parameterType, name, i, attributes));
        }

        return parameters;
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
}
