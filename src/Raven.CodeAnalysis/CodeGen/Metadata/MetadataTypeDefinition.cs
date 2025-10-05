using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Metadata;

internal sealed class MetadataTypeDefinition
{
    private readonly List<MetadataTypeDefinition> _nestedTypes = new();
    private readonly Dictionary<IMethodSymbol, MetadataMethodDefinition> _methods = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<IFieldSymbol, MetadataFieldDefinition> _fields = new(SymbolEqualityComparer.Default);
    private readonly Dictionary<IPropertySymbol, MetadataPropertyDefinition> _properties = new(SymbolEqualityComparer.Default);
    private ImmutableArray<ITypeSymbol> _interfaces = ImmutableArray<ITypeSymbol>.Empty;
    private ImmutableArray<AttributeData> _customAttributes = ImmutableArray<AttributeData>.Empty;

    public MetadataTypeDefinition(ITypeSymbol symbol)
    {
        Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
    }

    public ITypeSymbol Symbol { get; }

    public string Name => Symbol.MetadataName;

    public MetadataTypeKind Kind { get; private set; } = MetadataTypeKind.Unknown;

    public TypeAttributes Attributes { get; private set; }

    public ITypeSymbol? BaseType { get; private set; }

    public IReadOnlyList<MetadataTypeDefinition> NestedTypes => _nestedTypes;

    public ImmutableArray<ITypeSymbol> Interfaces => _interfaces;

    public ImmutableArray<AttributeData> CustomAttributes => _customAttributes;

    public IEnumerable<MetadataMethodDefinition> Methods => _methods.Values;

    public IEnumerable<MetadataFieldDefinition> Fields => _fields.Values;

    public IEnumerable<MetadataPropertyDefinition> Properties => _properties.Values;

    public void SetKind(MetadataTypeKind kind)
    {
        Kind = kind;
    }

    public void SetAttributes(TypeAttributes attributes)
    {
        Attributes = attributes;
    }

    public void SetBaseType(ITypeSymbol? baseType)
    {
        BaseType = baseType;
    }

    public void SetInterfaces(ImmutableArray<ITypeSymbol> interfaces)
    {
        _interfaces = interfaces.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : interfaces;
    }

    public void SetInterfaces(IEnumerable<ITypeSymbol> interfaces)
    {
        _interfaces = interfaces is ImmutableArray<ITypeSymbol> immutable
            ? immutable
            : interfaces.ToImmutableArray();
    }

    public void SetCustomAttributes(ImmutableArray<AttributeData> attributes)
    {
        _customAttributes = attributes.IsDefault ? ImmutableArray<AttributeData>.Empty : attributes;
    }

    internal void AddNestedType(MetadataTypeDefinition nested)
    {
        if (nested is null)
            throw new ArgumentNullException(nameof(nested));

        if (!_nestedTypes.Contains(nested))
            _nestedTypes.Add(nested);
    }

    public MetadataMethodDefinition GetOrAddMethodDefinition(IMethodSymbol method)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        if (!_methods.TryGetValue(method, out var definition))
        {
            definition = new MetadataMethodDefinition(method);
            _methods.Add(method, definition);
        }

        return definition;
    }

    public bool TryGetMethodDefinition(IMethodSymbol method, out MetadataMethodDefinition definition)
    {
        if (method is null)
            throw new ArgumentNullException(nameof(method));

        return _methods.TryGetValue(method, out definition);
    }

    public MetadataFieldDefinition GetOrAddFieldDefinition(IFieldSymbol field)
    {
        if (field is null)
            throw new ArgumentNullException(nameof(field));

        if (!_fields.TryGetValue(field, out var definition))
        {
            definition = new MetadataFieldDefinition(field);
            _fields.Add(field, definition);
        }

        return definition;
    }

    public bool TryGetFieldDefinition(IFieldSymbol field, out MetadataFieldDefinition definition)
    {
        if (field is null)
            throw new ArgumentNullException(nameof(field));

        return _fields.TryGetValue(field, out definition);
    }

    public MetadataPropertyDefinition GetOrAddPropertyDefinition(IPropertySymbol property)
    {
        if (property is null)
            throw new ArgumentNullException(nameof(property));

        if (!_properties.TryGetValue(property, out var definition))
        {
            definition = new MetadataPropertyDefinition(property);
            _properties.Add(property, definition);
        }

        return definition;
    }

    public bool TryGetPropertyDefinition(IPropertySymbol property, out MetadataPropertyDefinition definition)
    {
        if (property is null)
            throw new ArgumentNullException(nameof(property));

        return _properties.TryGetValue(property, out definition);
    }
}
