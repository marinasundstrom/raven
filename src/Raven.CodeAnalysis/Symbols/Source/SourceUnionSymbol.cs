using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceUnionSymbol : SourceNamedTypeSymbol, IUnionSymbol
{
    private ImmutableArray<ITypeSymbol> _caseTypes = ImmutableArray<ITypeSymbol>.Empty;
    private ImmutableArray<IUnionCaseTypeSymbol> _declaredCases = ImmutableArray<IUnionCaseTypeSymbol>.Empty;
    private ImmutableArray<ITypeSymbol> _memberTypes = ImmutableArray<ITypeSymbol>.Empty;

    public SourceUnionSymbol(
        string name,
        INamedTypeSymbol baseType,
        TypeKind typeKind,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility,
        string? metadataName = null)
        : base(name, baseType, typeKind, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, isSealed: true, declaredAccessibility: declaredAccessibility, metadataName: metadataName)
    {
    }

    public ImmutableArray<ITypeSymbol> CaseTypes => _caseTypes;

    public ImmutableArray<IUnionCaseTypeSymbol> DeclaredCaseTypes => _declaredCases;

    public ImmutableArray<ITypeSymbol> MemberTypes => _memberTypes;

    public IFieldSymbol DiscriminatorField { get; private set; } = null!;

    public IFieldSymbol PayloadField { get; private set; } = null!;

    public ImmutableArray<IFieldSymbol> PayloadFields { get; private set; } = ImmutableArray<IFieldSymbol>.Empty;

    internal void SetCases(IEnumerable<IUnionCaseTypeSymbol> cases)
    {
        _declaredCases = cases.ToImmutableArray();
    }

    internal void SetCaseTypes(IEnumerable<ITypeSymbol> caseTypes)
    {
        _caseTypes = caseTypes.ToImmutableArray();
    }

    internal void SetMemberTypes(IEnumerable<ITypeSymbol> memberTypes)
    {
        _memberTypes = memberTypes.ToImmutableArray();
    }

    internal void SetDiscriminatorField(SourceFieldSymbol discriminator)
    {
        DiscriminatorField = discriminator;
    }

    internal void SetPayloadFields(IEnumerable<SourceFieldSymbol> payloadFields)
    {
        PayloadFields = payloadFields.ToImmutableArray<IFieldSymbol>();
        if (!PayloadFields.IsDefaultOrEmpty)
            PayloadField = PayloadFields[0];
    }
}
