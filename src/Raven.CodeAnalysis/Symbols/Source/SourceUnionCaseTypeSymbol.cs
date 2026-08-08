using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceUnionCaseTypeSymbol : SourceNamedTypeSymbol, IUnionCaseTypeSymbol
{
    private readonly IUnionSymbol _union;
    private readonly INamedTypeSymbol _metadataContainingType;
    private ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol> _projectedUnionTypeParameters = ImmutableDictionary<ITypeParameterSymbol, ITypeParameterSymbol>.Empty.WithComparers(SymbolEqualityComparer.Default);

    public SourceUnionCaseTypeSymbol(
        string name,
        int ordinal,
        IUnionSymbol union,
        INamedTypeSymbol metadataContainingType,
        INamedTypeSymbol baseType,
        TypeKind typeKind,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(name, baseType, typeKind, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, isSealed: true, declaredAccessibility: declaredAccessibility)
    {
        _union = union;
        _metadataContainingType = metadataContainingType;
        Ordinal = ordinal;
    }

    public override string MetadataName
    {
        get
        {
            var name = Name;

            if (IsGenericType)
                name = $"{name}`{Arity}";

            return name;
        }
    }

    public ImmutableArray<IParameterSymbol> ConstructorParameters { get; private set; } = ImmutableArray<IParameterSymbol>.Empty;

    public int Ordinal { get; }

    public IUnionSymbol Union => _union;

    public INamedTypeSymbol MetadataContainingType => _metadataContainingType;

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
