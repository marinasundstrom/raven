using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal sealed class MetadataPropertyDefinition
{
    private ImmutableArray<MetadataParameterDefinition> _parameters = ImmutableArray<MetadataParameterDefinition>.Empty;
    private ImmutableArray<AttributeData> _customAttributes = ImmutableArray<AttributeData>.Empty;

    public MetadataPropertyDefinition(IPropertySymbol symbol)
    {
        Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
        PropertyType = symbol.Type;
    }

    public IPropertySymbol Symbol { get; }

    public string Name => Symbol.MetadataName;

    public PropertyAttributes Attributes { get; private set; }

    public ITypeSymbol PropertyType { get; private set; }

    public bool RequiresNullableAttribute { get; private set; }

    public ImmutableArray<MetadataParameterDefinition> Parameters => _parameters;

    public ImmutableArray<AttributeData> CustomAttributes => _customAttributes;

    public MetadataMethodDefinition? Getter { get; private set; }

    public MetadataMethodDefinition? Setter { get; private set; }

    public void SetAttributes(PropertyAttributes attributes)
    {
        Attributes = attributes;
    }

    public void SetPropertyType(ITypeSymbol propertyType)
    {
        PropertyType = propertyType ?? throw new ArgumentNullException(nameof(propertyType));
    }

    public void SetRequiresNullableAttribute(bool requiresNullableAttribute)
    {
        RequiresNullableAttribute = requiresNullableAttribute;
    }

    public void SetParameters(IEnumerable<MetadataParameterDefinition> parameters)
    {
        if (parameters is null)
            throw new ArgumentNullException(nameof(parameters));

        _parameters = parameters is ImmutableArray<MetadataParameterDefinition> immutable
            ? immutable
            : parameters.ToImmutableArray();
    }

    public void SetCustomAttributes(ImmutableArray<AttributeData> attributes)
    {
        _customAttributes = attributes.IsDefault ? ImmutableArray<AttributeData>.Empty : attributes;
    }

    public void SetGetAccessor(MetadataMethodDefinition? getter)
    {
        Getter = getter;
    }

    public void SetSetAccessor(MetadataMethodDefinition? setter)
    {
        Setter = setter;
    }
}
