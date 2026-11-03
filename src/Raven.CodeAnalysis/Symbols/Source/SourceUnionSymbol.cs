using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceUnionSymbol : SourceNamedTypeSymbol, UnionSymbol
{
    private ImmutableArray<TypeSymbol> _variants = ImmutableArray<TypeSymbol>.Empty;
    private ImmutableArray<UnionCaseTypeSymbol> _declaredCases = ImmutableArray<UnionCaseTypeSymbol>.Empty;
    private ImmutableArray<TypeSymbol> _memberTypes = ImmutableArray<TypeSymbol>.Empty;
    private bool _contentMayBeNull;

    public SourceUnionSymbol(
        string name,
        NamedTypeSymbol baseType,
        TypeKind typeKind,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility,
        string? metadataName = null)
        : base(name, baseType, typeKind, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, isSealed: true, declaredAccessibility: declaredAccessibility, metadataName: metadataName)
    {
    }

    public override bool IsReadOnly => IsValueType;

    public ImmutableArray<TypeSymbol> Variants => _variants;

    public ImmutableArray<UnionCaseTypeSymbol> DeclaredCaseTypes => _declaredCases;

    public ImmutableArray<TypeSymbol> MemberTypes => _memberTypes;

    public bool ContentMayBeNull => _contentMayBeNull || _memberTypes.Any(UnionContentNullability.IsNullableContentType);

    public FieldSymbol DiscriminatorField { get; private set; } = null!;

    public FieldSymbol PayloadField { get; private set; } = null!;

    public ImmutableArray<FieldSymbol> PayloadFields { get; private set; } = ImmutableArray<FieldSymbol>.Empty;

    internal SynthesizedUnionCompanionTypeSymbol? CompanionType { get; private set; }

    internal NamedTypeSymbol GetMetadataCaseContainer()
    {
        if (!IsGenericType)
            return this;

        return CompanionType ??= new SynthesizedUnionCompanionTypeSymbol(this);
    }

    internal void SetCases(IEnumerable<UnionCaseTypeSymbol> cases)
    {
        _declaredCases = cases.ToImmutableArray();
    }

    internal void SetVariants(IEnumerable<TypeSymbol> variants)
    {
        _variants = variants.ToImmutableArray();
    }

    internal void SetMemberTypes(IEnumerable<TypeSymbol> memberTypes)
    {
        _memberTypes = memberTypes.ToImmutableArray();
    }

    internal void SetContentMayBeNull(bool contentMayBeNull)
    {
        _contentMayBeNull = contentMayBeNull;
    }

    internal void SetDiscriminatorField(SourceFieldSymbol discriminator)
    {
        DiscriminatorField = discriminator;
    }

    internal void SetPayloadFields(IEnumerable<SourceFieldSymbol> payloadFields)
    {
        PayloadFields = payloadFields.ToImmutableArray<FieldSymbol>();
        if (!PayloadFields.IsDefaultOrEmpty)
            PayloadField = PayloadFields[0];
    }
}
