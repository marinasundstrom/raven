using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class IntersectionTypeSymbolImplementation : SymbolBase, IntersectionTypeSymbol
{
    private IntersectionTypeSymbolImplementation(ImmutableArray<TypeSymbol> types)
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
            throw new ArgumentException("An intersection requires at least one constituent.", nameof(types));

        // Only nominal inheritance removes bounds. Conversion operators and numeric
        // widening do not establish membership of the same value in both types.
        var normalized = constituents.Where(candidate => !constituents.Any(other =>
            !ReferenceEquals(candidate, other) && IsNominalSubtype(other, candidate) &&
            !IsNominalSubtype(candidate, other))).ToImmutableArray();

        return normalized.Length == 1 ? normalized[0] : new IntersectionTypeSymbolImplementation(normalized);

        void Add(TypeSymbol type)
        {
            ArgumentNullException.ThrowIfNull(type);
            if (type is IntersectionTypeSymbol intersection)
            {
                foreach (var constituent in intersection.ConstituentTypes)
                    Add(constituent);
            }
            else if (type is not NamedTypeSymbol { TypeKind: TypeKind.Interface } &&
                type is not NamedTypeSymbol { TypeKind: TypeKind.Class, IsReferenceType: true, IsClosed: false })
            {
                throw new ArgumentException("Intersection bounds must be interfaces or reference classes that permit subclassing.", nameof(types));
            }
            else if (!constituents.Any(existing => SymbolEqualityComparer.Default.Equals(existing, type)))
            {
                constituents.Add(type);
            }
        }
    }

    private static bool IsNominalSubtype(TypeSymbol subtype, TypeSymbol supertype)
    {
        if (subtype is not NamedTypeSymbol || supertype is not NamedTypeSymbol)
            return false;

        var visited = new HashSet<TypeSymbol>(SymbolEqualityComparer.Default);
        for (var baseType = subtype.BaseType; baseType is not null && visited.Add(baseType); baseType = baseType.BaseType)
        {
            if (SymbolEqualityComparer.Default.Equals(baseType, supertype))
                return true;
        }

        return subtype.AllInterfaces.Any(iface => SymbolEqualityComparer.Default.Equals(iface, supertype));
    }

    public ImmutableArray<TypeSymbol> ConstituentTypes { get; }
    public override string Name => string.Join(" & ", ConstituentTypes.Select(type => type.Name));
    public override string MetadataName => string.Empty;
    public override bool IsImplicitlyDeclared => true;
    public TypeKind TypeKind => TypeKind.Intersection;
    public SpecialType SpecialType => SpecialType.None;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => ConstituentTypes.All(type => type.IsReferenceType);
    public bool IsValueType => ConstituentTypes.Any(type => type.IsValueType);
    public NamedTypeSymbol? BaseType => null;
    public TypeSymbol OriginalDefinition => this;

    public ImmutableArray<NamedTypeSymbol> Interfaces => GetInterfaces(includeInherited: false);
    public ImmutableArray<NamedTypeSymbol> AllInterfaces => GetInterfaces(includeInherited: true);

    private ImmutableArray<NamedTypeSymbol> GetInterfaces(bool includeInherited)
    {
        var result = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<Symbol>(SymbolEqualityComparer.Default);
        foreach (var type in ConstituentTypes)
        {
            if (type is NamedTypeSymbol { TypeKind: TypeKind.Interface } iface && seen.Add(iface))
                result.Add(iface);
            foreach (var implemented in includeInherited ? type.AllInterfaces : type.Interfaces)
            {
                if (seen.Add(implemented))
                    result.Add(implemented);
            }
        }

        return result.ToImmutable();
    }

    public ImmutableArray<Symbol> GetMembers() => CollectMembers(name: null);
    public ImmutableArray<Symbol> GetMembers(string name) => CollectMembers(name);

    private ImmutableArray<Symbol> CollectMembers(string? name)
    {
        var seen = new HashSet<Symbol>(SymbolEqualityComparer.Default);
        var result = ImmutableArray.CreateBuilder<Symbol>();
        foreach (var type in ConstituentTypes)
        {
            foreach (var member in name is null ? type.GetMembers() : type.GetMembers(name))
            {
                if (seen.Add(member))
                    result.Add(member);
            }
        }

        return result.ToImmutable();
    }

    public TypeSymbol? LookupType(string name)
    {
        var types = GetMembers(name).OfType<TypeSymbol>().ToArray();
        return types.Length == 1 ? types[0] : null;
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        var members = GetMembers(name);
        symbol = members.Length == 1 ? members[0] : null;
        return symbol is not null;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitIntersectionType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitIntersectionType(this);
}
