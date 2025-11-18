using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceDiscriminatedUnionSymbol : SourceNamedTypeSymbol, IDiscriminatedUnionSymbol
{
    private ImmutableArray<IDiscriminatedUnionCaseSymbol> _cases = ImmutableArray<IDiscriminatedUnionCaseSymbol>.Empty;

    public SourceDiscriminatedUnionSymbol(
        string name,
        INamedTypeSymbol baseType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(name, baseType, TypeKind.Struct, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, isSealed: true, declaredAccessibility: declaredAccessibility)
    {
    }

    public ImmutableArray<IDiscriminatedUnionCaseSymbol> Cases => _cases;

    public IFieldSymbol DiscriminatorField { get; private set; } = null!;

    public IFieldSymbol PayloadField { get; private set; } = null!;

    internal void SetCases(IEnumerable<IDiscriminatedUnionCaseSymbol> cases)
    {
        _cases = cases.ToImmutableArray();
    }

    internal void InitializeStorageFields(SourceFieldSymbol discriminator, SourceFieldSymbol payload)
    {
        DiscriminatorField = discriminator;
        PayloadField = payload;
    }
}
