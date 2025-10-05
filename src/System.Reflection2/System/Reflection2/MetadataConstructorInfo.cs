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
    private readonly Lazy<MethodSignature<Type>> _signature;
    private readonly Lazy<IReadOnlyList<MetadataParameterInfo>> _parameters;

    internal MetadataConstructorInfo(MetadataType declaringType, MethodDefinitionHandle handle)
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

    public override ParameterInfo[] GetParameters() => _parameters.Value.Cast<ParameterInfo>().ToArray();

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override bool IsDefined(Type attributeType, bool inherit) => false;

    public override string Name => _reader.GetString(_definition.Name);

    public override Type? DeclaringType => _declaringType;

    public override Type? ReflectedType => DeclaringType;

    public override object? Invoke(object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
        => throw new NotSupportedException("Invocation is not supported in metadata-only context.");

    public override object Invoke(BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
        => throw new NotSupportedException("Invocation is not supported in metadata-only context.");

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override bool IsGenericMethod => false;

    public override bool IsGenericMethodDefinition => false;

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
