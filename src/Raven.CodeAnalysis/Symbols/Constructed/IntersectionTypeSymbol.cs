using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class IntersectionTypeSymbol : Symbol, IIntersectionTypeSymbol
{
    private IntersectionTypeSymbol(ImmutableArray<ITypeSymbol> types)
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
            throw new ArgumentException("An intersection requires at least one constituent.", nameof(types));

        // Only nominal inheritance removes bounds. Conversion operators and numeric
        // widening do not establish membership of the same value in both types.
        var normalized = constituents.Where(candidate => !constituents.Any(other =>
            !ReferenceEquals(candidate, other) && IsNominalSubtype(other, candidate) &&
            !IsNominalSubtype(candidate, other))).ToImmutableArray();

        return normalized.Length == 1 ? normalized[0] : new IntersectionTypeSymbol(normalized);

        void Add(ITypeSymbol type)
        {
            ArgumentNullException.ThrowIfNull(type);
            if (type is IIntersectionTypeSymbol intersection)
            {
                foreach (var constituent in intersection.ConstituentTypes)
                    Add(constituent);
            }
            else if (!constituents.Any(existing => SymbolEqualityComparer.Default.Equals(existing, type)))
            {
                constituents.Add(type);
            }
        }
    }

    private static bool IsNominalSubtype(ITypeSymbol subtype, ITypeSymbol supertype)
    {
        if (subtype is not INamedTypeSymbol || supertype is not INamedTypeSymbol)
            return false;

        var visited = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
        for (var baseType = subtype.BaseType; baseType is not null && visited.Add(baseType); baseType = baseType.BaseType)
        {
            if (SymbolEqualityComparer.Default.Equals(baseType, supertype))
                return true;
        }

        return subtype.AllInterfaces.Any(iface => SymbolEqualityComparer.Default.Equals(iface, supertype));
    }

    public ImmutableArray<ITypeSymbol> ConstituentTypes { get; }
    public override string Name => string.Join(" & ", ConstituentTypes.Select(type => type.Name));
    public override string MetadataName => string.Empty;
    public override bool IsImplicitlyDeclared => true;
    public TypeKind TypeKind => TypeKind.Intersection;
    public SpecialType SpecialType => SpecialType.None;
    public bool IsNamespace => false;
    public bool IsType => true;
    public bool IsReferenceType => ConstituentTypes.All(type => type.IsReferenceType);
    public bool IsValueType => ConstituentTypes.Any(type => type.IsValueType);
    public INamedTypeSymbol? BaseType => null;
    public ITypeSymbol OriginalDefinition => this;

    public ImmutableArray<INamedTypeSymbol> Interfaces => GetInterfaces(includeInherited: false);
    public ImmutableArray<INamedTypeSymbol> AllInterfaces => GetInterfaces(includeInherited: true);

    private ImmutableArray<INamedTypeSymbol> GetInterfaces(bool includeInherited)
    {
        var result = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        foreach (var type in ConstituentTypes)
        {
            if (type is INamedTypeSymbol { TypeKind: TypeKind.Interface } iface && seen.Add(iface))
                result.Add(iface);
            foreach (var implemented in includeInherited ? type.AllInterfaces : type.Interfaces)
            {
                if (seen.Add(implemented))
                    result.Add(implemented);
            }
        }

        return result.ToImmutable();
    }

    public ImmutableArray<ISymbol> GetMembers() => CollectMembers(name: null);
    public ImmutableArray<ISymbol> GetMembers(string name) => CollectMembers(name);

    private ImmutableArray<ISymbol> CollectMembers(string? name)
    {
        var seen = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var result = ImmutableArray.CreateBuilder<ISymbol>();
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

    public ITypeSymbol? LookupType(string name)
    {
        var types = GetMembers(name).OfType<ITypeSymbol>().ToArray();
        return types.Length == 1 ? types[0] : null;
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        var members = GetMembers(name);
        symbol = members.Length == 1 ? members[0] : null;
        return symbol is not null;
    }

    public override void Accept(SymbolVisitor visitor) => visitor.VisitIntersectionType(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitIntersectionType(this);
}
