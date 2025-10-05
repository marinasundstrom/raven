using System;
using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal sealed class MetadataFieldDefinition
{
    private ImmutableArray<AttributeData> _customAttributes = ImmutableArray<AttributeData>.Empty;

    public MetadataFieldDefinition(IFieldSymbol symbol)
    {
        Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
        FieldType = symbol.Type;
    }

    public IFieldSymbol Symbol { get; }

    public string Name => Symbol.Name;

    public FieldAttributes Attributes { get; private set; }

    public ITypeSymbol FieldType { get; private set; }

    public bool RequiresNullableAttribute { get; private set; }

    public object? ConstantValue { get; private set; }

    public ImmutableArray<AttributeData> CustomAttributes => _customAttributes;

    public void SetAttributes(FieldAttributes attributes)
    {
        Attributes = attributes;
    }

    public void SetFieldType(ITypeSymbol fieldType)
    {
        FieldType = fieldType ?? throw new ArgumentNullException(nameof(fieldType));
    }

    public void SetRequiresNullableAttribute(bool requiresNullableAttribute)
    {
        RequiresNullableAttribute = requiresNullableAttribute;
    }

    public void SetConstantValue(object? value)
    {
        ConstantValue = value;
    }

    public void SetCustomAttributes(ImmutableArray<AttributeData> attributes)
    {
        _customAttributes = attributes.IsDefault ? ImmutableArray<AttributeData>.Empty : attributes;
    }
}
