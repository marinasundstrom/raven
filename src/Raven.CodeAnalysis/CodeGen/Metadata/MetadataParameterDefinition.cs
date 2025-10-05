using System;
using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal sealed class MetadataParameterDefinition
{
    private ImmutableArray<AttributeData> _customAttributes = ImmutableArray<AttributeData>.Empty;

    public MetadataParameterDefinition(IParameterSymbol symbol)
    {
        Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
    }

    public IParameterSymbol Symbol { get; }

    public string Name => Symbol.Name;

    public ParameterAttributes Attributes { get; private set; }

    public bool RequiresNullableAttribute { get; private set; }

    public ImmutableArray<AttributeData> CustomAttributes => _customAttributes;

    public void SetAttributes(ParameterAttributes attributes)
    {
        Attributes = attributes;
    }

    public void SetRequiresNullableAttribute(bool requiresNullableAttribute)
    {
        RequiresNullableAttribute = requiresNullableAttribute;
    }

    public void SetCustomAttributes(ImmutableArray<AttributeData> attributes)
    {
        _customAttributes = attributes.IsDefault ? ImmutableArray<AttributeData>.Empty : attributes;
    }
}
