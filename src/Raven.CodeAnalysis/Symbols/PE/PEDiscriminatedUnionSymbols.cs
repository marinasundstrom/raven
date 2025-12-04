using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PEDiscriminatedUnionSymbol : PENamedTypeSymbol, IDiscriminatedUnionSymbol
{
    private ImmutableArray<IDiscriminatedUnionCaseSymbol>? _cases;
    private IFieldSymbol? _discriminatorField;
    private IFieldSymbol? _payloadField;

    public PEDiscriminatedUnionSymbol(
        TypeResolver typeResolver,
        System.Reflection.TypeInfo typeInfo,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(typeResolver, typeInfo, containingSymbol, containingType, containingNamespace, locations)
    {
    }

    public ImmutableArray<IDiscriminatedUnionCaseSymbol> Cases
    {
        get
        {
            if (_cases is not null)
                return _cases.Value;

            _cases = GetMembers()
                .OfType<IDiscriminatedUnionCaseSymbol>()
                .ToImmutableArray();

            return _cases.Value;
        }
    }

    public IFieldSymbol DiscriminatorField =>
        _discriminatorField ??= FindUnionField("Tag")
            ?? throw new InvalidOperationException($"Missing discriminator field on discriminated union '{Name}'.");

    public IFieldSymbol PayloadField =>
        _payloadField ??= FindUnionField("Payload")
            ?? throw new InvalidOperationException($"Missing payload field on discriminated union '{Name}'.");

    private IFieldSymbol? FindUnionField(string target)
    {
        var fields = GetMembers()
            .OfType<IFieldSymbol>();

        foreach (var field in fields)
        {
            var normalized = NormalizeFieldName(field.Name);
            if (normalized == target)
                return field;
        }

        return null;
    }

    private static string NormalizeFieldName(string name)
    {
        // Drop common punctuation so variants like "<Tag>", "_Tag" or "Tag" all match.
        Span<char> buffer = stackalloc char[name.Length];
        var index = 0;

        foreach (var ch in name)
        {
            if (ch is '<' or '>' or '_')
                continue;

            buffer[index++] = ch;
        }

        return new string(buffer[..index]);
    }
}

internal sealed class PEDiscriminatedUnionCaseSymbol : PENamedTypeSymbol, IDiscriminatedUnionCaseSymbol
{
    private IDiscriminatedUnionSymbol? _union;
    private readonly IDiscriminatedUnionSymbol? _unionFromAttribute;
    private ImmutableArray<IParameterSymbol>? _constructorParameters;
    private int? _ordinal;

    public PEDiscriminatedUnionCaseSymbol(
        TypeResolver typeResolver,
        System.Reflection.TypeInfo typeInfo,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        IDiscriminatedUnionSymbol? unionFromAttribute)
        : base(typeResolver, typeInfo, containingSymbol, containingType, containingNamespace, locations)
    {
        _unionFromAttribute = unionFromAttribute;
    }

    public IDiscriminatedUnionSymbol Union
    {
        get
        {
            if (_union is not null)
                return _union;

            if (ContainingType is IDiscriminatedUnionSymbol containingUnion)
                return _union = containingUnion;

            if (_unionFromAttribute is not null)
                return _union = _unionFromAttribute;

            var resolvedFromAttribute = ResolveUnionFromAttribute();
            if (resolvedFromAttribute is not null)
                return _union = resolvedFromAttribute;

            throw new InvalidOperationException($"Could not resolve discriminated union for '{Name}'.");
        }
    }

    public ImmutableArray<IParameterSymbol> ConstructorParameters
    {
        get
        {
            if (_constructorParameters is not null)
                return _constructorParameters.Value;

            var constructor = GetMembers()
                .OfType<IMethodSymbol>()
                .FirstOrDefault(m => m.Name == ".ctor");

            _constructorParameters = constructor?.Parameters ?? ImmutableArray<IParameterSymbol>.Empty;
            return _constructorParameters.Value;
        }
    }

    public int Ordinal
    {
        get
        {
            if (_ordinal is not null)
                return _ordinal.Value;

            var cases = Union.Cases;
            var index = cases.IndexOf(this, SymbolEqualityComparer.Default);
            _ordinal = index >= 0 ? index : 0;
            return _ordinal.Value;
        }
    }

    private IDiscriminatedUnionSymbol? ResolveUnionFromAttribute()
    {
        foreach (var attribute in PENamedTypeSymbol.GetCustomAttributesSafe(_typeInfo))
        {
            var attributeName = GetAttributeTypeName(attribute);
            if (attributeName != "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute")
                continue;

            if (!TryGetAttributeConstructorTypeArgument(attribute, out var unionType))
                continue;

            var resolvedUnion = _typeResolver.ResolveType(unionType);
            return resolvedUnion as IDiscriminatedUnionSymbol;
        }

        return null;
    }

    private static string? GetAttributeTypeName(CustomAttributeData attribute)
    {
        try
        {
            return attribute.AttributeType.FullName;
        }
        catch (ArgumentException)
        {
            return null;
        }
    }

    private static bool TryGetAttributeConstructorTypeArgument(CustomAttributeData attribute, out Type unionType)
    {
        unionType = null!;

        try
        {
            if (attribute.ConstructorArguments is [{ Value: Type unionTypeValue }])
            {
                unionType = unionTypeValue;
                return true;
            }
        }
        catch (ArgumentException)
        {
            return false;
        }

        return false;
    }
}
