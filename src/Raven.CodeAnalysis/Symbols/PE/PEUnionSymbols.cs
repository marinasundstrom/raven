using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PEUnionCompanionSymbol : PENamedTypeSymbol, UnionCompanionSymbol
{
    private const string RavenUnionCompanionAttributeMetadataName = "Raven.Runtime.CompilerServices.RavenUnionCompanionAttribute";
    private UnionSymbol? _associatedUnion;
    private bool _associationResolving;

    public PEUnionCompanionSymbol(
        ReflectionTypeLoader reflectionTypeLoader,
        System.Reflection.TypeInfo typeInfo,
        Symbol containingSymbol,
        NamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType: null, containingNamespace, locations, addAsMember: false)
    {
    }

    public bool TryGetAssociatedUnion(out UnionSymbol union)
    {
        if (_associatedUnion is null && !_associationResolving)
        {
            _associationResolving = true;
            try
            {
                _associatedUnion = ResolveAssociatedUnion();
            }
            finally
            {
                _associationResolving = false;
            }
        }

        union = _associatedUnion!;
        return union is not null;
    }

    private UnionSymbol? ResolveAssociatedUnion()
    {
        foreach (var attribute in GetCustomAttributesSafe(_typeInfo))
        {
            if (GetAttributeTypeName(attribute) != RavenUnionCompanionAttributeMetadataName)
                continue;

            try
            {
                if (attribute.ConstructorArguments is not [{ Value: string unionMetadataName }])
                    continue;

                if (ContainingAssembly?.GetTypeByMetadataName(unionMetadataName) is UnionSymbol assemblyUnion)
                {
                    return assemblyUnion;
                }
            }
            catch (Exception exception) when (
                exception is ArgumentException or
                    BadImageFormatException or
                    TypeLoadException or
                    System.IO.FileNotFoundException)
            {
                return null;
            }
        }

        return null;
    }
}

internal sealed class PEUnionSymbol : PENamedTypeSymbol, UnionSymbol
{
    private const string RavenUnionCaseAttributeMetadataName = "Raven.Runtime.CompilerServices.RavenUnionCaseAttribute";

    private ImmutableArray<UnionCaseTypeSymbol>? _cases;
    private ImmutableArray<TypeSymbol>? _variants;
    private ImmutableArray<TypeSymbol>? _memberTypes;
    private bool? _contentMayBeNull;
    private FieldSymbol? _discriminatorField;
    private FieldSymbol? _payloadField;

    public PEUnionSymbol(
        ReflectionTypeLoader reflectionTypeLoader,
        System.Reflection.TypeInfo typeInfo,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
    {
    }

    public ImmutableArray<TypeSymbol> Variants
    {
        get
        {
            if (_variants is not null)
                return _variants.Value;

            var declaredCases = DeclaredCaseTypes;
            _variants = declaredCases.IsDefaultOrEmpty
                ? MemberTypes
                : declaredCases.Cast<TypeSymbol>().ToImmutableArray();
            return _variants.Value;
        }
    }

    public ImmutableArray<UnionCaseTypeSymbol> DeclaredCaseTypes
    {
        get
        {
            if (_cases is not null)
                return _cases.Value;

            if (UnionFacts.GetMemberProvider(this) is not null)
                return (_cases = ImmutableArray<UnionCaseTypeSymbol>.Empty).Value;

            var cases = GetDeclaredCaseTypesFromRavenMetadata();

            if (cases.IsDefaultOrEmpty)
            {
                cases = GetMembers()
                    .OfType<UnionCaseTypeSymbol>()
                    .ToImmutableArray();
            }

            if (cases.IsDefaultOrEmpty && ContainingNamespace is not null)
            {
                cases = ContainingNamespace
                    .GetAllMembersRecursive()
                    .OfType<UnionCaseTypeSymbol>()
                    .Where(caseSymbol => SymbolEqualityComparer.Default.Equals(caseSymbol.Union, this))
                    .Distinct(SymbolEqualityComparer.Default)
                    .OfType<UnionCaseTypeSymbol>()
                    .ToImmutableArray();
            }

            _cases = cases;
            return _cases.Value;
        }
    }

    internal bool TryGetDeclaredCaseType(string logicalCaseName, out UnionCaseTypeSymbol caseType)
    {
        caseType = DeclaredCaseTypes.FirstOrDefault(@case =>
            string.Equals(@case.Name, logicalCaseName, StringComparison.Ordinal))!;
        return caseType is not null;
    }

    private ImmutableArray<UnionCaseTypeSymbol> GetDeclaredCaseTypesFromRavenMetadata()
    {
        var builder = ImmutableArray.CreateBuilder<(int Ordinal, UnionCaseTypeSymbol CaseType)>();

        foreach (var attribute in GetCustomAttributesSafe(_typeInfo))
        {
            if (GetAttributeTypeName(attribute) != RavenUnionCaseAttributeMetadataName)
                continue;

            if (!TryReadRavenUnionCaseAttribute(attribute, out var caseTypeMetadataName, out _, out var ordinal))
                continue;

            if (TryResolveRavenUnionCaseType(caseTypeMetadataName, out var caseType))
                builder.Add((ordinal, caseType));
        }

        if (builder.Count == 0)
            return ImmutableArray<UnionCaseTypeSymbol>.Empty;

        return builder
            .OrderBy(static item => item.Ordinal)
            .Select(static item => item.CaseType)
            .Distinct(SymbolEqualityComparer.Default)
            .OfType<UnionCaseTypeSymbol>()
            .ToImmutableArray();
    }

    private bool TryResolveRavenUnionCaseType(string caseTypeMetadataName, out UnionCaseTypeSymbol caseType)
    {
        return TryResolveCaseTypeByMetadataName(caseTypeMetadataName, out caseType);
    }

    private static bool TryReadRavenUnionCaseAttribute(
        CustomAttributeData attribute,
        out string caseTypeMetadataName,
        out string logicalCaseName,
        out int ordinal)
    {
        caseTypeMetadataName = string.Empty;
        logicalCaseName = string.Empty;
        ordinal = 0;

        try
        {
            if (attribute.ConstructorArguments is not
                [
                { Value: string caseTypeMetadataNameValue },
                { Value: string nameValue },
                { Value: int ordinalValue }
                ])
            {
                return false;
            }

            caseTypeMetadataName = caseTypeMetadataNameValue;
            logicalCaseName = nameValue;
            ordinal = ordinalValue;
            return true;
        }
        catch (ArgumentException)
        {
            return false;
        }
    }

    private bool TryResolveCaseTypeByMetadataName(string metadataName, out UnionCaseTypeSymbol caseType)
    {
        var namedType = ContainingAssembly?.GetTypeByMetadataName(metadataName) as NamedTypeSymbol;
        return TryAsUnionCaseType(namedType, out caseType);
    }

    private bool TryAsUnionCaseType(NamedTypeSymbol? namedType, out UnionCaseTypeSymbol caseType)
    {
        if (namedType is UnionCaseTypeSymbol existingCaseType)
        {
            caseType = existingCaseType;
            return true;
        }

        if (namedType is PENamedTypeSymbol peType)
        {
            caseType = new PEUnionCaseSymbol(
                _reflectionTypeLoader,
                peType.GetTypeInfo(),
                this,
                peType.ContainingType ?? this,
                peType.ContainingNamespace,
                peType.Locations.ToArray(),
                this);
            return true;
        }

        caseType = null!;
        return false;
    }

    public FieldSymbol DiscriminatorField =>
        _discriminatorField ??= FindUnionField(UnionFieldUtilities.IsTagFieldName)
            ?? throw new InvalidOperationException($"Missing discriminator field on discriminated union '{Name}'.");

    public ImmutableArray<TypeSymbol> MemberTypes
    {
        get
        {
            if (_memberTypes is not null)
                return _memberTypes.Value;

            var declaredCases = DeclaredCaseTypes;
            _memberTypes = declaredCases.IsDefaultOrEmpty
                ? GetRuntimeUnionMemberTypes()
                : declaredCases.Cast<TypeSymbol>().ToImmutableArray();
            return _memberTypes.Value;
        }
    }

    public bool ContentMayBeNull => _contentMayBeNull ??= ComputeContentMayBeNull();

    public FieldSymbol PayloadField =>
        _payloadField ??= FindUnionField(UnionFieldUtilities.IsPayloadFieldName)
            ?? throw new InvalidOperationException($"Missing payload field on discriminated union '{Name}'.");

    private FieldSymbol? FindUnionField(Func<string, bool> predicate)
    {
        var fields = GetMembers()
            .OfType<FieldSymbol>();

        foreach (var field in fields)
        {
            if (predicate(field.Name))
                return field;
        }

        return null;
    }

    private ImmutableArray<TypeSymbol> GetRuntimeUnionMemberTypes()
    {
        var constructorTypes = ImmutableArray.CreateBuilder<TypeSymbol>();

        if (UnionFacts.GetMemberProvider(this) is not null)
        {
            foreach (var factory in UnionFacts.GetProviderFactories(this))
                AddMemberType(constructorTypes, factory.Parameters[0].GetByRefElementType(), ref _contentMayBeNull);
            return constructorTypes.ToImmutable();
        }

        foreach (var constructor in Constructors)
        {
            if (constructor.IsStatic ||
                constructor.DeclaredAccessibility != Accessibility.Public ||
                constructor.Parameters.Length != 1)
            {
                continue;
            }

            AddMemberType(constructorTypes, constructor.Parameters[0].GetByRefElementType(), ref _contentMayBeNull);
        }

        if (constructorTypes.Count > 0)
            return constructorTypes.ToImmutable();

        var accessPatternTypes = ImmutableArray.CreateBuilder<TypeSymbol>();

        foreach (var method in GetMembers("TryGetValue").OfType<MethodSymbol>())
        {
            if (method.IsStatic ||
                method.DeclaredAccessibility != Accessibility.Public ||
                method.Parameters.Length != 1 ||
                method.Parameters[0].RefKind != RefKind.Out)
            {
                continue;
            }

            AddMemberType(accessPatternTypes, method.Parameters[0].GetByRefElementType(), ref _contentMayBeNull);
        }

        return accessPatternTypes.ToImmutable();
    }

    private bool ComputeContentMayBeNull()
    {
        _ = MemberTypes;
        return _contentMayBeNull.GetValueOrDefault();
    }

    private static void AddMemberType(
        ImmutableArray<TypeSymbol>.Builder builder,
        TypeSymbol memberType,
        ref bool? contentMayBeNull)
    {
        memberType = UnionContentNullability.GetNonNullContentType(memberType, out var memberMayBeNull);
        contentMayBeNull = contentMayBeNull.GetValueOrDefault() || memberMayBeNull;

        if (builder.Any(existing => SymbolEqualityComparer.Default.Equals(existing, memberType)))
            return;

        builder.Add(memberType);
    }

}

internal sealed class PEUnionCaseSymbol : PENamedTypeSymbol, UnionCaseTypeSymbol
{
    private UnionSymbol? _union;
    private readonly UnionSymbol? _inferredUnion;
    private string? _logicalCaseName;
    private ImmutableArray<ParameterSymbol>? _constructorParameters;
    private int? _ordinal;
    private readonly NamedTypeSymbol _metadataContainingType;

    public PEUnionCaseSymbol(
        ReflectionTypeLoader reflectionTypeLoader,
        System.Reflection.TypeInfo typeInfo,
        UnionSymbol semanticUnion,
        NamedTypeSymbol metadataContainingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        UnionSymbol? inferredUnion)
        : base(reflectionTypeLoader, typeInfo, semanticUnion, (NamedTypeSymbol)semanticUnion, containingNamespace, locations,
        addAsMember: false)
    {
        _inferredUnion = inferredUnion;
        _metadataContainingType = metadataContainingType;
    }

    public override string Name => _logicalCaseName ??= ComputeLogicalCaseName();

    public UnionSymbol Union
    {
        get
        {
            if (_union is not null)
                return _union;

            if (ContainingType is UnionSymbol containingUnion)
                return _union = containingUnion;

            if (_inferredUnion is not null)
                return _union = _inferredUnion;

            throw new InvalidOperationException($"Could not resolve discriminated union for '{Name}'.");
        }
    }

    public NamedTypeSymbol MetadataContainingType => _metadataContainingType;

    public ImmutableArray<ParameterSymbol> ConstructorParameters
    {
        get
        {
            if (_constructorParameters is not null)
                return _constructorParameters.Value;

            var constructor = GetMembers()
                .OfType<MethodSymbol>()
                .FirstOrDefault(m => m.Name == ".ctor");

            _constructorParameters = constructor?.Parameters ?? ImmutableArray<ParameterSymbol>.Empty;
            return _constructorParameters.Value;
        }
    }

    public int Ordinal
    {
        get
        {
            if (_ordinal is not null)
                return _ordinal.Value;

            var cases = Union.Variants;
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

    private UnionSymbol? TryGetKnownUnion()
    {
        if (_union is not null)
            return _union;

        if (ContainingType is UnionSymbol containingUnion)
            return containingUnion;

        if (_inferredUnion is not null)
            return _inferredUnion;

        return null;
    }
}
