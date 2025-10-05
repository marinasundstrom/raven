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
    private readonly MetadataType _declaringType;
    private readonly MetadataReader _reader;
    private readonly MethodDefinitionHandle _handle;
    private readonly MethodDefinition _definition;
    private readonly Lazy<MethodSignature<Type>> _signature;
    private readonly Lazy<IReadOnlyList<MetadataParameterInfo>> _parameters;

    internal MetadataMethodInfo(MetadataType declaringType, MethodDefinitionHandle handle)
    {
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _reader = declaringType.Reader;
        _handle = handle;
        _definition = _reader.GetMethodDefinition(handle);
        _signature = new Lazy<MethodSignature<Type>>(DecodeSignature);
        _parameters = new Lazy<IReadOnlyList<MetadataParameterInfo>>(DecodeParameters);
    }

    public MethodDefinitionHandle Handle => _handle;

    public override MethodAttributes Attributes => _definition.Attributes;

    public override CallingConventions CallingConvention => (CallingConventions)_signature.Value.Header.CallingConvention;

    public override bool ContainsGenericParameters => _definition.GetGenericParameters().Any();

    public override RuntimeMethodHandle MethodHandle => throw new NotSupportedException();

    public override MethodImplAttributes GetMethodImplementationFlags() => _definition.ImplAttributes;

    public override ParameterInfo ReturnParameter => new MetadataParameterInfo(_signature.Value.ReturnType, null, -1, ParameterAttributes.None);

    public override Type ReturnType => _signature.Value.ReturnType;

    public override ICustomAttributeProvider ReturnTypeCustomAttributes => this;

    public override Type? DeclaringType => _declaringType;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override MethodInfo GetBaseDefinition() => this;

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override bool IsDefined(Type attributeType, bool inherit) => false;

    public override MethodInfo MakeGenericMethod(params Type[] typeArguments)
        => throw new NotSupportedException("Generic method construction is not supported in metadata-only context.");

    public override MethodInfo GetGenericMethodDefinition()
        => throw new NotSupportedException();

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override ParameterInfo[] GetParameters() => _parameters.Value.Cast<ParameterInfo>().ToArray();

    public override object? Invoke(object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
        => throw new NotSupportedException("Invocation is not supported in metadata-only context.");

    internal bool IsConstructorDefinition => Name is ".ctor" or ".cctor";

    private MethodSignature<Type> DecodeSignature()
    {
        var provider = new MetadataSignatureTypeProvider(_declaringType.MetadataModule, _declaringType.GetGenericArguments());
        return _definition.DecodeSignature(provider, _declaringType);
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
}
