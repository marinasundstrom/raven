using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceUnionCaseTypeSymbol : SourceNamedTypeSymbol, UnionCaseTypeSymbol
{
    private readonly UnionSymbol _union;
    private readonly NamedTypeSymbol _metadataContainingType;
    private ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol> _projectedUnionTypeParameters = ImmutableDictionary<TypeParameterSymbol, TypeParameterSymbol>.Empty.WithComparers(SymbolEqualityComparer.Default);

    public SourceUnionCaseTypeSymbol(
        string name,
        int ordinal,
        UnionSymbol union,
        NamedTypeSymbol metadataContainingType,
        NamedTypeSymbol baseType,
        TypeKind typeKind,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
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

    public ImmutableArray<ParameterSymbol> ConstructorParameters { get; private set; } = ImmutableArray<ParameterSymbol>.Empty;

    public override bool IsReadOnly => IsValueType && ConstructorParameters.All(parameter => !parameter.IsMutable);

    public int Ordinal { get; }

    public UnionSymbol Union => _union;

    public NamedTypeSymbol MetadataContainingType => _metadataContainingType;

    internal void SetConstructorParameters(IEnumerable<SourceParameterSymbol> parameters)
    {
        ConstructorParameters = parameters.Cast<ParameterSymbol>().ToImmutableArray();
    }

    internal void SetProjectedUnionTypeParameters(IEnumerable<(TypeParameterSymbol CaseTypeParameter, TypeParameterSymbol UnionTypeParameter)> mappings)
    {
        var builder = ImmutableDictionary.CreateBuilder<TypeParameterSymbol, TypeParameterSymbol>(SymbolEqualityComparer.Default);
        foreach (var (caseTypeParameter, unionTypeParameter) in mappings)
            builder[caseTypeParameter] = unionTypeParameter;

        _projectedUnionTypeParameters = builder.ToImmutable();
    }

    internal bool TryGetProjectedUnionTypeParameter(TypeParameterSymbol caseTypeParameter, out TypeParameterSymbol unionTypeParameter)
        => _projectedUnionTypeParameters.TryGetValue(caseTypeParameter, out unionTypeParameter!);
}
