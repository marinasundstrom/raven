namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
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
    private readonly Lazy<ParameterDefaultValue>? _defaultValue;
    private readonly MetadataModule? _module;
    private readonly MethodBase? _declaringMember;
    private readonly bool _isReturnParameter;
    private readonly Type[] _requiredCustomModifiers;
    private readonly Type[] _optionalCustomModifiers;

    internal MetadataParameterInfo(
        Type parameterType,
        string? name,
        int position,
        ParameterAttributes attributes,
        MetadataModule? module = null,
        ParameterHandle parameterHandle = default,
        MetadataType? declaringTypeContext = null,
        IReadOnlyList<Type>? genericMethodArguments = null,
        MethodBase? declaringMember = null,
        bool isReturnParameter = false,
        Type[]? requiredCustomModifiers = null,
        Type[]? optionalCustomModifiers = null)
    {
        _parameterType = parameterType;
        _name = name;
        _position = position;
        _attributes = attributes;
        _module = module;
        _declaringMember = declaringMember;
        _isReturnParameter = isReturnParameter;
        _requiredCustomModifiers = NormalizeModifiers(requiredCustomModifiers);
        _optionalCustomModifiers = NormalizeModifiers(optionalCustomModifiers);
        if (module is not null && !parameterHandle.IsNil)
        {
            var parameter = module.Reader.GetParameter(parameterHandle);
            _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(module, parameter.GetCustomAttributes(), declaringTypeContext, genericMethodArguments));
            _defaultValue = new Lazy<ParameterDefaultValue>(() => DecodeDefaultValue(parameter, parameterType));
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

            var value = _defaultValue.Value;
            return value.Kind switch
            {
                ParameterDefaultValueKind.None => DBNull.Value,
                ParameterDefaultValueKind.Missing => Missing.Value,
                ParameterDefaultValueKind.Value => value.Value,
                _ => DBNull.Value,
            };
        }
    }

    public override object? RawDefaultValue => DefaultValue;

    public override bool HasDefaultValue => _defaultValue?.Value.Kind == ParameterDefaultValueKind.Value;

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes?.Value ?? Array.Empty<CustomAttributeData>();

    public override Type[] GetRequiredCustomModifiers() => _requiredCustomModifiers;

    public override Type[] GetOptionalCustomModifiers() => _optionalCustomModifiers;

    public override object[] GetCustomAttributes(bool inherit)
    {
        var bridge = _module?.RuntimeBridge;
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

        var bridge = _module?.RuntimeBridge;
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

        var bridge = _module?.RuntimeBridge;
        if (bridge is not null)
        {
            return bridge.IsDefined(this, attributeType, inherit);
        }

        return (_customAttributes?.Value ?? Array.Empty<CustomAttributeData>()).Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    }

    internal MethodBase? DeclaringMember => _declaringMember;

    internal bool IsReturnParameter => _isReturnParameter;
    private ParameterDefaultValue DecodeDefaultValue(Parameter parameter, Type parameterType)
    {
        var constantHandle = parameter.GetDefaultValue();
        if (!constantHandle.IsNil)
        {
            if (_module is null)
            {
                return ParameterDefaultValue.None;
            }

            var reader = _module.Reader;
            var constant = reader.GetConstant(constantHandle);
            var blobReader = reader.GetBlobReader(constant.Value);
            var rawValue = blobReader.ReadConstant(constant.TypeCode);

            if (rawValue is null)
            {
                return ParameterDefaultValue.FromValue(null);
            }

            if (MetadataConstantDecoder.IsEnumLike(parameterType))
            {
                var enumType = MetadataConstantDecoder.TryResolveRuntimeEnumType(parameterType);
                if (enumType is not null)
                {
                    return ParameterDefaultValue.FromValue(Enum.ToObject(enumType, rawValue));
                }
            }

            if (parameterType == typeof(decimal) && rawValue is double doubleValue)
            {
                return ParameterDefaultValue.FromValue(Convert.ToDecimal(doubleValue));
            }

            return ParameterDefaultValue.FromValue(rawValue);
        }

        if (_customAttributes is not null)
        {
            var attributes = _customAttributes.Value;
            if (MetadataConstantDecoder.TryDecodeDecimalConstant(attributes, out var decimalValue))
            {
                return ParameterDefaultValue.FromValue(decimalValue);
            }

            if (MetadataConstantDecoder.TryDecodeDateTimeConstant(attributes, out var dateTimeValue))
            {
                return ParameterDefaultValue.FromValue(dateTimeValue);
            }

            if (TryDecodeDefaultParameterValue(attributes, out var defaultValue))
            {
                return ParameterDefaultValue.FromValue(defaultValue);
            }

            if (HasOptionalAttribute(attributes))
            {
                return ParameterDefaultValue.Missing;
            }
        }

        if ((parameter.Attributes & ParameterAttributes.Optional) != 0)
        {
            return ParameterDefaultValue.Missing;
        }

        return ParameterDefaultValue.None;
    }

    private static Type[] NormalizeModifiers(Type[]? modifiers)
    {
        if (modifiers is null || modifiers.Length == 0)
        {
            return Type.EmptyTypes;
        }

        var copy = new Type[modifiers.Length];
        Array.Copy(modifiers, copy, modifiers.Length);
        return copy;
    }

    private bool TryDecodeDefaultParameterValue(IList<CustomAttributeData> attributes, out object? value)
    {
        foreach (var attribute in attributes)
        {
            if (!string.Equals(attribute.AttributeType.FullName, "System.Runtime.InteropServices.DefaultParameterValueAttribute", StringComparison.Ordinal))
            {
                continue;
            }

            var args = attribute.ConstructorArguments;
            if (args.Count == 1)
            {
                value = args[0].Value;
                return true;
            }
        }

        value = null;
        return false;
    }

    private static bool HasOptionalAttribute(IList<CustomAttributeData> attributes)
    {
        foreach (var attribute in attributes)
        {
            if (string.Equals(attribute.AttributeType.FullName, "System.Runtime.InteropServices.OptionalAttribute", StringComparison.Ordinal))
            {
                return true;
            }
        }

        return false;
    }

    private readonly struct ParameterDefaultValue
    {
        private ParameterDefaultValue(ParameterDefaultValueKind kind, object? value)
        {
            Kind = kind;
            Value = value;
        }

        public ParameterDefaultValueKind Kind { get; }

        public object? Value { get; }

        public static ParameterDefaultValue None => new(ParameterDefaultValueKind.None, null);

        public static ParameterDefaultValue Missing => new(ParameterDefaultValueKind.Missing, null);

        public static ParameterDefaultValue FromValue(object? value) => new(ParameterDefaultValueKind.Value, value);
    }

    private enum ParameterDefaultValueKind
    {
        None,
        Missing,
        Value,
    }
}
