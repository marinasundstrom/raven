using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class StructuralUnionTypeSymbolImplementation : SymbolBase, StructuralUnionTypeSymbol
{
    private StructuralUnionTypeSymbolImplementation(ImmutableArray<TypeSymbol> types)
        : base(SymbolKind.Type, string.Empty, null, null, null, [], [],
            Accessibility.NotApplicable, addAsMember: false)
    {
        ConstituentTypes = types;
    }

    internal static TypeSymbol Create(IEnumerable<TypeSymbol> types)
    {
        ArgumentNullException.ThrowIfNull(types);
        var constituents = new List<TypeSymbol>();
        foreach (var type in types)
            Add(type);

        if (constituents.Count == 0)
            throw new ArgumentException("A structural union requires at least one constituent.", nameof(types));

        // Preserve nominal alternatives; native descriptors are not reduced to a carrier.
        var normalized = constituents.ToImmutableArray();
        return normalized.Length == 1 ? normalized[0] : new StructuralUnionTypeSymbolImplementation(normalized);

        void Add(TypeSymbol type)
        {
            ArgumentNullException.ThrowIfNull(type);
            if (type is StructuralUnionTypeSymbol union)
            {
                foreach (var constituent in union.ConstituentTypes)
                    Add(constituent);
            }
            else if (!CompositionTypeBounds.IsUnionAlternative(type))
            {
                throw new ArgumentException("Structural union alternatives require nominal types or bound type parameters.", nameof(types));
            }
            else if (!constituents.Any(existing => SymbolEqualityComparer.Default.Equals(existing, type)))
            {
                constituents.Add(type);
            }
        }
    }

    public ImmutableArray<TypeSymbol> ConstituentTypes { get; }
    public override string Name => string.Join(" | ", ConstituentTypes.Select(type => type.Name));
    public override string MetadataName => string.Empty;
    public override bool IsImplicitlyDeclared => true;
    public TypeKind TypeKind => TypeKind.StructuralUnion;
    public SpecialType SpecialType => SpecialType.None;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => false;
    public bool IsValueType => true;
    public NamedTypeSymbol? BaseType => null;
    public TypeSymbol OriginalDefinition => this;

    // Member merging/dispatch is outside the imported-call profile.
    public ImmutableArray<NamedTypeSymbol> Interfaces => [];
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<Symbol> GetMembers() => [];
    public ImmutableArray<Symbol> GetMembers(string name) => [];
    public TypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out Symbol? symbol) { symbol = null; return false; }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitStructuralUnionType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitStructuralUnionType(this);
}
