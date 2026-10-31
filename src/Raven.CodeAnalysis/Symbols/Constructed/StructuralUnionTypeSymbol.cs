using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class StructuralUnionTypeSymbol : Symbol, IStructuralUnionTypeSymbol
{
    private StructuralUnionTypeSymbol(ImmutableArray<ITypeSymbol> types)
        : base(SymbolKind.Type, string.Empty, null, null, null, [], [],
            Accessibility.NotApplicable, addAsMember: false)
    {
        ConstituentTypes = types;
    }

    internal static ITypeSymbol Create(IEnumerable<ITypeSymbol> types)
    {
        ArgumentNullException.ThrowIfNull(types);
        var constituents = new List<ITypeSymbol>();
        foreach (var type in types)
            Add(type);

        if (constituents.Count == 0)
            throw new ArgumentException("A structural union requires at least one constituent.", nameof(types));

        // Preserve nominal alternatives; native descriptors are not reduced to a carrier.
        var normalized = constituents.ToImmutableArray();
        return normalized.Length == 1 ? normalized[0] : new StructuralUnionTypeSymbol(normalized);

        void Add(ITypeSymbol type)
        {
            ArgumentNullException.ThrowIfNull(type);
            if (type is IStructuralUnionTypeSymbol union)
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

    public ImmutableArray<ITypeSymbol> ConstituentTypes { get; }
    public override string Name => string.Join(" | ", ConstituentTypes.Select(type => type.Name));
    public override string MetadataName => string.Empty;
    public override bool IsImplicitlyDeclared => true;
    public TypeKind TypeKind => TypeKind.StructuralUnion;
    public SpecialType SpecialType => SpecialType.None;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => false;
    public bool IsValueType => true;
    public INamedTypeSymbol? BaseType => null;
    public ITypeSymbol OriginalDefinition => this;

    // Member merging/dispatch is outside the imported-call profile.
    public ImmutableArray<INamedTypeSymbol> Interfaces => [];
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => [];
    public ImmutableArray<ISymbol> GetMembers() => [];
    public ImmutableArray<ISymbol> GetMembers(string name) => [];
    public ITypeSymbol? LookupType(string name) => null;
    public bool IsMemberDefined(string name, out ISymbol? symbol) { symbol = null; return false; }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitStructuralUnionType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitStructuralUnionType(this);
}
