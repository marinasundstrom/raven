using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal sealed class MetadataMethodDefinition
{
    private ImmutableArray<MetadataParameterDefinition> _parameters = ImmutableArray<MetadataParameterDefinition>.Empty;
    private ImmutableArray<AttributeData> _customAttributes = ImmutableArray<AttributeData>.Empty;
    private ImmutableArray<AttributeData> _returnAttributes = ImmutableArray<AttributeData>.Empty;

    public MetadataMethodDefinition(IMethodSymbol symbol)
    {
        Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
        ReturnType = symbol.ReturnType;
    }

    public IMethodSymbol Symbol { get; }

    public string Name => Symbol.MetadataName;

    public MethodAttributes Attributes { get; private set; }

    public MethodImplAttributes ImplementationAttributes { get; private set; } = MethodImplAttributes.IL;

    public ITypeSymbol ReturnType { get; private set; }

    public bool RequiresNullableAttributeOnReturn { get; private set; }

    public ImmutableArray<MetadataParameterDefinition> Parameters => _parameters;

    public ImmutableArray<AttributeData> CustomAttributes => _customAttributes;

    public ImmutableArray<AttributeData> ReturnAttributes => _returnAttributes;

    public bool IsEntryPointCandidate { get; private set; }

    public void SetAttributes(MethodAttributes attributes)
    {
        Attributes = attributes;
    }

    public void SetImplementationAttributes(MethodImplAttributes attributes)
    {
        ImplementationAttributes = attributes;
    }

    public void SetReturnType(ITypeSymbol returnType)
    {
        ReturnType = returnType ?? throw new ArgumentNullException(nameof(returnType));
    }

    public void SetRequiresNullableAttributeOnReturn(bool requiresNullableAttribute)
    {
        RequiresNullableAttributeOnReturn = requiresNullableAttribute;
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

    public void SetReturnAttributes(ImmutableArray<AttributeData> attributes)
    {
        _returnAttributes = attributes.IsDefault ? ImmutableArray<AttributeData>.Empty : attributes;
    }

    public void SetIsEntryPointCandidate(bool isEntryPointCandidate)
    {
        IsEntryPointCandidate = isEntryPointCandidate;
    }
}
