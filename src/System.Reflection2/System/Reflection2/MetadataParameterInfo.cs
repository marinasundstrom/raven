namespace System.Reflection2;

using System;
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

    internal MetadataParameterInfo(Type parameterType, string? name, int position, ParameterAttributes attributes)
    {
        _parameterType = parameterType;
        _name = name;
        _position = position;
        _attributes = attributes;
    }

    public override Type ParameterType => _parameterType;

    public override string? Name => _name;

    public override int Position => _position;

    public override ParameterAttributes Attributes => _attributes;

    public override object? DefaultValue => DBNull.Value;

    public override object? RawDefaultValue => DBNull.Value;

    public override bool HasDefaultValue => false;
}
