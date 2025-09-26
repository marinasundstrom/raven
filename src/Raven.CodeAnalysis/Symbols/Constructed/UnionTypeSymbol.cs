using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal partial class UnionTypeSymbol : SourceSymbol, IUnionTypeSymbol
{
    private readonly ImmutableArray<ITypeSymbol> _types;

    public UnionTypeSymbol(IEnumerable<ITypeSymbol> types, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(SymbolKind.Type, string.Empty, containingSymbol, containingType, containingNamespace, locations, [])
    {
        _types = types is ImmutableArray<ITypeSymbol> array ? array : ImmutableArray.CreateRange(types);

        BaseType = ComputeBaseType(_types);

        TypeKind = TypeKind.Union;
    }

    public override string Name => string.Join(" | ", Types.Select(x => x.ToDisplayStringKeywordAware(SymbolDisplayFormat.FullyQualifiedFormat)));

    public IEnumerable<ITypeSymbol> Types => _types;

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public ImmutableArray<INamedTypeSymbol> Interfaces =>
        BaseType?.Interfaces ?? ImmutableArray<INamedTypeSymbol>.Empty;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        BaseType?.AllInterfaces ?? ImmutableArray<INamedTypeSymbol>.Empty;

    public ImmutableArray<ISymbol> GetMembers()
    {
        return BaseType?.GetMembers() ?? ImmutableArray<ISymbol>.Empty;
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return BaseType?.GetMembers(name) ?? ImmutableArray<ISymbol>.Empty;
    }

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        return Name;
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        if (BaseType is not null)
            return BaseType.IsMemberDefined(name, out symbol);

        symbol = null;
        return false;
    }

    private static INamedTypeSymbol? ComputeBaseType(ImmutableArray<ITypeSymbol> types)
    {
        var members = FlattenTypes(types)
            .Select(Normalize)
            .ToImmutableArray();

        if (members.Length == 0)
            return null;

        var nonNullMembers = members.Where(t => t.TypeKind != TypeKind.Null).ToImmutableArray();
        if (nonNullMembers.Length == 0)
            return TryGetObjectType(members);

        var candidates = EnumerateHierarchy(nonNullMembers[0]).ToList();
        if (candidates.Count == 0)
            return TryGetObjectType(nonNullMembers);

        var comparer = SymbolEqualityComparer.Default;

        foreach (var type in nonNullMembers.Skip(1))
        {
            var hierarchy = new HashSet<INamedTypeSymbol>(EnumerateHierarchy(type), comparer);
            candidates.RemoveAll(candidate => !hierarchy.Contains(candidate));

            if (candidates.Count == 0)
                break;
        }

        return candidates.FirstOrDefault() ?? TryGetObjectType(nonNullMembers);
    }

    private static IEnumerable<ITypeSymbol> FlattenTypes(IEnumerable<ITypeSymbol> types)
    {
        foreach (var type in types)
        {
            if (type is IUnionTypeSymbol union)
            {
                foreach (var nested in FlattenTypes(union.Types))
                    yield return nested;
            }
            else
            {
                yield return type;
            }
        }
    }

    private static ITypeSymbol Normalize(ITypeSymbol type)
    {
        while (true)
        {
            if (type.IsAlias && type.UnderlyingSymbol is ITypeSymbol alias)
            {
                type = alias;
                continue;
            }

            if (type is LiteralTypeSymbol literal)
            {
                type = literal.UnderlyingType;
                continue;
            }

            break;
        }

        return type;
    }

    private static IEnumerable<INamedTypeSymbol> EnumerateHierarchy(ITypeSymbol type)
    {
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

        for (ITypeSymbol? current = Normalize(type); current is not null; current = GetNext(current))
        {
            if (current is INamedTypeSymbol named && seen.Add(named))
                yield return named;
        }
    }

    private static ITypeSymbol? GetNext(ITypeSymbol type)
    {
        return type switch
        {
            NullableTypeSymbol nullable => Normalize(nullable.UnderlyingType),
            _ => type.BaseType is { } baseType ? Normalize(baseType) : null
        };
    }

    private static INamedTypeSymbol? TryGetObjectType(IEnumerable<ITypeSymbol> types)
    {
        foreach (var type in types)
        {
            foreach (var candidate in EnumerateHierarchy(type))
            {
                if (candidate.SpecialType == SpecialType.System_Object)
                    return candidate;
            }

            if (type.ContainingAssembly?.GetTypeByMetadataName("System.Object") is { } objectType)
                return objectType;
        }

        return null;
    }
}
