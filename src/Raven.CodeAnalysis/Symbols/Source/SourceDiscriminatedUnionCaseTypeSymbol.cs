using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceDiscriminatedUnionCaseTypeSymbol : SourceNamedTypeSymbol, IDiscriminatedUnionCaseSymbol
{
    private readonly IDiscriminatedUnionSymbol _union;
    private readonly string _metadataBaseName;
    private ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> _projectedUnionTypeParameters = ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty.WithComparers(SymbolEqualityComparer.Default);

    public SourceDiscriminatedUnionCaseTypeSymbol(
        string name,
        string metadataBaseName,
        int ordinal,
        IDiscriminatedUnionSymbol union,
        INamedTypeSymbol baseType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(name, baseType, TypeKind.Struct, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, isSealed: true, declaredAccessibility: declaredAccessibility)
    {
        _union = union;
        _metadataBaseName = metadataBaseName;
        Ordinal = ordinal;
    }

    public override string MetadataName
    {
        get
        {
            var name = _metadataBaseName;

            if (IsGenericType)
                name = $"{name}`{Arity}";

            if (ContainingType is INamedTypeSymbol containingType)
                return $"{containingType.MetadataName}+{name}";

            if (ContainingNamespace is { IsGlobalNamespace: false } containingNamespace)
            {
                var namespaceMetadata = containingNamespace.MetadataName;
                return string.IsNullOrEmpty(namespaceMetadata)
                    ? name
                    : $"{namespaceMetadata}.{name}";
            }

            return name;
        }
    }

    public ImmutableArray<IParameterSymbol> ConstructorParameters { get; private set; } = ImmutableArray<IParameterSymbol>.Empty;

    public int Ordinal { get; }

    public IDiscriminatedUnionSymbol Union => _union;

    internal void SetConstructorParameters(IEnumerable<SourceParameterSymbol> parameters)
    {
        ConstructorParameters = parameters.Cast<IParameterSymbol>().ToImmutableArray();
    }

    internal void SetProjectedUnionTypeParameters(IEnumerable<(ITypeParameterSymbol CaseTypeParameter, ITypeParameterSymbol UnionTypeParameter)> mappings)
    {
        var builder = ImmutableDictionary.CreateBuilder<ITypeParameterSymbol, ITypeParameterSymbol>(SymbolEqualityComparer.Default);
        foreach (var (caseTypeParameter, unionTypeParameter) in mappings)
            builder[caseTypeParameter] = unionTypeParameter;

        _projectedUnionTypeParameters = builder.ToImmutable();
    }

    internal bool TryGetProjectedUnionTypeParameter(ITypeParameterSymbol caseTypeParameter, out ITypeParameterSymbol unionTypeParameter)
        => _projectedUnionTypeParameters.TryGetValue(caseTypeParameter, out unionTypeParameter!);
}
