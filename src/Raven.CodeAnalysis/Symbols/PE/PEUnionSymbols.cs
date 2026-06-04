using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PEUnionSymbol : PENamedTypeSymbol, IUnionSymbol
{
    private const string RavenUnionCaseAttributeMetadataName = "Raven.Runtime.CompilerServices.RavenUnionCaseAttribute";

    private ImmutableArray<IUnionCaseTypeSymbol>? _cases;
    private ImmutableArray<ITypeSymbol>? _caseTypes;
    private ImmutableArray<ITypeSymbol>? _memberTypes;
    private bool? _contentMayBeNull;
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
    {
        get
        {
            if (_caseTypes is not null)
                return _caseTypes.Value;

            var declaredCases = DeclaredCaseTypes;
            _caseTypes = declaredCases.IsDefaultOrEmpty
                ? MemberTypes
                : declaredCases.Cast<ITypeSymbol>().ToImmutableArray();
            return _caseTypes.Value;
        }
    }

    public ImmutableArray<IUnionCaseTypeSymbol> DeclaredCaseTypes
    {
        get
        {
            if (_cases is not null)
                return _cases.Value;

            var cases = GetDeclaredCaseTypesFromRavenMetadata();

            if (cases.IsDefaultOrEmpty)
            {
                cases = GetMembers()
                    .OfType<IUnionCaseTypeSymbol>()
                    .ToImmutableArray();
            }

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

    internal bool TryGetDeclaredCaseType(string logicalCaseName, out IUnionCaseTypeSymbol caseType)
    {
        if (_cases is { } loadedCases)
        {
            caseType = loadedCases.FirstOrDefault(@case =>
                string.Equals(@case.Name, logicalCaseName, StringComparison.Ordinal))!;
            return caseType is not null;
        }

        foreach (var metadataName in GetCandidateCaseMetadataNames(logicalCaseName))
        {
            if (TryResolveCaseMetadataType(metadataName, out var candidate))
            {
                caseType = candidate;
                return true;
            }
        }

        caseType = null!;
        return false;
    }

    private ImmutableArray<IUnionCaseTypeSymbol> GetDeclaredCaseTypesFromRavenMetadata()
    {
        var builder = ImmutableArray.CreateBuilder<(int Ordinal, IUnionCaseTypeSymbol CaseType)>();

        foreach (var attribute in GetCustomAttributesSafe(_typeInfo))
        {
            if (GetAttributeTypeName(attribute) != RavenUnionCaseAttributeMetadataName)
                continue;

            if (!TryReadRavenUnionCaseAttribute(attribute, out var caseTypeMetadataName, out var logicalCaseName, out var ordinal))
                continue;

            if (TryResolveRavenUnionCaseType(caseTypeMetadataName, logicalCaseName, out var caseType))
                builder.Add((ordinal, caseType));
        }

        if (builder.Count == 0)
            return ImmutableArray<IUnionCaseTypeSymbol>.Empty;

        return builder
            .OrderBy(static item => item.Ordinal)
            .Select(static item => item.CaseType)
            .Distinct(SymbolEqualityComparer.Default)
            .OfType<IUnionCaseTypeSymbol>()
            .ToImmutableArray();
    }

    private bool TryResolveRavenUnionCaseType(string caseTypeMetadataName, string logicalCaseName, out IUnionCaseTypeSymbol caseType)
    {
        if (TryResolveCaseTypeByMetadataName(caseTypeMetadataName, out caseType))
            return true;

        foreach (var metadataName in GetCandidateCaseMetadataNames(logicalCaseName))
        {
            if (TryResolveCaseMetadataType(metadataName, out caseType))
                return true;
        }

        caseType = null!;
        return false;
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

    private bool TryResolveCaseMetadataType(string metadataName, out IUnionCaseTypeSymbol caseType)
    {
        var namespaceName = ContainingNamespace?.ToMetadataName() ?? string.Empty;
        var fullName = string.IsNullOrEmpty(namespaceName)
            ? metadataName
            : namespaceName + "." + metadataName;

        return TryResolveCaseTypeByMetadataName(fullName, out caseType);
    }

    private bool TryResolveCaseTypeByMetadataName(string metadataName, out IUnionCaseTypeSymbol caseType)
    {
        var namedType = ContainingAssembly?.GetTypeByMetadataName(metadataName) as INamedTypeSymbol;
        return TryAsUnionCaseType(namedType, out caseType);
    }

    private bool TryAsUnionCaseType(INamedTypeSymbol? namedType, out IUnionCaseTypeSymbol caseType)
    {
        if (namedType is IUnionCaseTypeSymbol existingCaseType)
        {
            caseType = existingCaseType;
            return true;
        }

        if (namedType is PENamedTypeSymbol peType)
        {
            caseType = new PEUnionCaseSymbol(
                _reflectionTypeLoader,
                peType.GetTypeInfo(),
                peType.ContainingSymbol ?? peType.ContainingNamespace ?? ContainingSymbol!,
                peType.ContainingType,
                peType.ContainingNamespace,
                peType.Locations.ToArray(),
                this);
            return true;
        }

        caseType = null!;
        return false;
    }

    private IEnumerable<string> GetCandidateCaseMetadataNames(string logicalCaseName)
    {
        var aritySuffixStart = MetadataName.IndexOf('`', StringComparison.Ordinal);
        var aritySuffix = aritySuffixStart >= 0 ? MetadataName[aritySuffixStart..] : string.Empty;

        yield return UnionFacts.GetCaseMetadataBaseName(Name, logicalCaseName) + aritySuffix;
        yield return UnionFacts.GetCaseMetadataBaseName(Name, logicalCaseName);
        yield return Name + logicalCaseName + aritySuffix;
        yield return Name + logicalCaseName;
    }

    public IFieldSymbol DiscriminatorField =>
        _discriminatorField ??= FindUnionField(UnionFieldUtilities.IsTagFieldName)
            ?? throw new InvalidOperationException($"Missing discriminator field on discriminated union '{Name}'.");

    public ImmutableArray<ITypeSymbol> MemberTypes
    {
        get
        {
            if (_memberTypes is not null)
                return _memberTypes.Value;

            var declaredCases = DeclaredCaseTypes;
            _memberTypes = declaredCases.IsDefaultOrEmpty
                ? GetRuntimeUnionMemberTypes()
                : declaredCases.Cast<ITypeSymbol>().ToImmutableArray();
            return _memberTypes.Value;
        }
    }

    public bool ContentMayBeNull => _contentMayBeNull ??= ComputeContentMayBeNull();

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

    private ImmutableArray<ITypeSymbol> GetRuntimeUnionMemberTypes()
    {
        var builder = ImmutableArray.CreateBuilder<ITypeSymbol>();

        foreach (var constructor in Constructors)
        {
            if (constructor.IsStatic ||
                constructor.DeclaredAccessibility != Accessibility.Public ||
                constructor.Parameters.Length != 1)
            {
                continue;
            }

            AddMemberType(builder, constructor.Parameters[0].Type);
        }

        foreach (var method in GetMembers("TryGetValue").OfType<IMethodSymbol>())
        {
            if (method.IsStatic ||
                method.DeclaredAccessibility != Accessibility.Public ||
                method.Parameters.Length != 1 ||
                method.Parameters[0].RefKind != RefKind.Out)
            {
                continue;
            }

            AddMemberType(builder, method.Parameters[0].GetByRefElementType());
        }

        return builder.ToImmutable();
    }

    private bool ComputeContentMayBeNull()
    {
        if (MemberTypes.Any(IsNullableContentType))
            return true;

        return GetMembers("Value")
            .OfType<IPropertySymbol>()
            .Any(property => IsNullableContentType(property.Type));
    }

    private static void AddMemberType(ImmutableArray<ITypeSymbol>.Builder builder, ITypeSymbol memberType)
    {
        if (builder.Any(existing => SymbolEqualityComparer.Default.Equals(existing, memberType)))
            return;

        builder.Add(memberType);
    }

    private static bool IsNullableContentType(ITypeSymbol type)
        => type.TypeKind == TypeKind.Null ||
           type.IsNullable ||
           type is INamedTypeSymbol { SpecialType: SpecialType.System_Nullable_T };
}

internal sealed class PEUnionCaseSymbol : PENamedTypeSymbol, IUnionCaseTypeSymbol
{
    private IUnionSymbol? _union;
    private readonly IUnionSymbol? _inferredUnion;
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
        IUnionSymbol? inferredUnion)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations,
        addAsMember: false)
    {
        _inferredUnion = inferredUnion;
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

            if (_inferredUnion is not null)
                return _union = _inferredUnion;

            var resolvedFromMetadataName = ResolveUnionFromMetadataName();
            if (resolvedFromMetadataName is not null)
                return _union = resolvedFromMetadataName;

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

        if (_inferredUnion is not null)
            return _inferredUnion;

        return ResolveUnionFromMetadataName();
    }

    private IUnionSymbol? ResolveUnionFromMetadataName()
    {
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
}
