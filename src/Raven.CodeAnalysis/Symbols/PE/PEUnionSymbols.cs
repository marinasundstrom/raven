using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PEUnionSymbol : PENamedTypeSymbol, IUnionSymbol
{
    private ImmutableArray<IUnionCaseTypeSymbol>? _cases;
    private ImmutableArray<ITypeSymbol>? _caseTypes;
    private ImmutableArray<ITypeSymbol>? _memberTypes;
    private IFieldSymbol? _discriminatorField;
    private IFieldSymbol? _payloadField;

    public PEUnionSymbol(
        ReflectionTypeLoader reflectionTypeLoader,
        System.Reflection.TypeInfo typeInfo,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
    {
    }

    public ImmutableArray<ITypeSymbol> CaseTypes
        => _caseTypes ??= DeclaredCaseTypes.Cast<ITypeSymbol>().ToImmutableArray();

    public ImmutableArray<IUnionCaseTypeSymbol> DeclaredCaseTypes
    {
        get
        {
            if (_cases is not null)
                return _cases.Value;

            var cases = GetMembers()
                .OfType<IUnionCaseTypeSymbol>()
                .ToImmutableArray();

            if (cases.IsDefaultOrEmpty && ContainingNamespace is not null)
            {
                cases = ContainingNamespace
                    .GetAllMembersRecursive()
                    .OfType<IUnionCaseTypeSymbol>()
                    .Where(caseSymbol => SymbolEqualityComparer.Default.Equals(caseSymbol.Union, this))
                    .Distinct(SymbolEqualityComparer.Default)
                    .OfType<IUnionCaseTypeSymbol>()
                    .ToImmutableArray();
            }

            _cases = cases;
            return _cases.Value;
        }
    }

    public IFieldSymbol DiscriminatorField =>
        _discriminatorField ??= FindUnionField(UnionFieldUtilities.IsTagFieldName)
            ?? throw new InvalidOperationException($"Missing discriminator field on discriminated union '{Name}'.");

    public ImmutableArray<ITypeSymbol> MemberTypes =>
        _memberTypes ??= DeclaredCaseTypes.Cast<ITypeSymbol>().ToImmutableArray();

    public IFieldSymbol PayloadField =>
        _payloadField ??= FindUnionField(UnionFieldUtilities.IsPayloadFieldName)
            ?? throw new InvalidOperationException($"Missing payload field on discriminated union '{Name}'.");

    private IFieldSymbol? FindUnionField(Func<string, bool> predicate)
    {
        var fields = GetMembers()
            .OfType<IFieldSymbol>();

        foreach (var field in fields)
        {
            if (predicate(field.Name))
                return field;
        }

        return null;
    }
}

internal sealed class PEUnionCaseSymbol : PENamedTypeSymbol, IUnionCaseTypeSymbol
{
    private IUnionSymbol? _union;
    private readonly IUnionSymbol? _unionFromAttribute;
    private string? _logicalCaseName;
    private ImmutableArray<IParameterSymbol>? _constructorParameters;
    private int? _ordinal;

    public PEUnionCaseSymbol(
        ReflectionTypeLoader reflectionTypeLoader,
        System.Reflection.TypeInfo typeInfo,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        IUnionSymbol? unionFromAttribute)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations,
        addAsMember: false)
    {
        _unionFromAttribute = unionFromAttribute;
    }

    public override string Name => _logicalCaseName ??= ComputeLogicalCaseName();

    public IUnionSymbol Union
    {
        get
        {
            if (_union is not null)
                return _union;

            if (ContainingType is IUnionSymbol containingUnion)
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

            var cases = Union.CaseTypes;
            var index = cases.IndexOf(this, SymbolEqualityComparer.Default);
            _ordinal = index >= 0 ? index : 0;
            return _ordinal.Value;
        }
    }

    private string ComputeLogicalCaseName()
    {
        var rawName = base.Name;
        var union = TryGetKnownUnion();
        if (union is null)
            return rawName;

        _ = UnionFacts.TryGetLogicalCaseNameFromMetadata(union.Name, rawName, out var logicalCaseName);
        return logicalCaseName;
    }

    private IUnionSymbol? TryGetKnownUnion()
    {
        if (_union is not null)
            return _union;

        if (ContainingType is IUnionSymbol containingUnion)
            return containingUnion;

        if (_unionFromAttribute is not null)
            return _unionFromAttribute;

        return ResolveUnionFromAttribute();
    }

    private IUnionSymbol? ResolveUnionFromAttribute()
    {
        foreach (var attribute in PENamedTypeSymbol.GetCustomAttributesSafe(_typeInfo))
        {
            var attributeName = GetAttributeTypeName(attribute);
            if (attributeName is not
                ("System.Runtime.CompilerServices.UnionCaseAttribute" or
                 "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute"))
                continue;

            if (!TryGetAttributeConstructorTypeArgument(attribute, out var unionType))
                continue;

            var resolvedUnion = _reflectionTypeLoader.ResolveType(unionType);
            return resolvedUnion as IUnionSymbol;
        }

        // Fallback for metadata that omits explicit case->union attributes:
        // infer the carrier from the scoped case metadata name (e.g. Result_Ok).
        if (ContainingNamespace is not null)
        {
            var rawCaseName = base.Name;

            var inferredUnion = ContainingNamespace
                .GetAllMembersRecursive()
                .OfType<IUnionSymbol>()
                .FirstOrDefault(unionSymbol =>
                    UnionFacts.TryGetLogicalCaseNameFromMetadata(
                        unionSymbol.Name,
                        rawCaseName,
                        out _));

            if (inferredUnion is not null)
                return inferredUnion;
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
