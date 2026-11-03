using Raven.CodeAnalysis;

public static partial class DocumentationGenerator
{
    private static readonly Dictionary<TypeSymbol, UnionSymbol> CompanionOwners = new(SymbolEqualityComparer.Default);

    private static IEnumerable<NamedTypeSymbol> GetUnionCompanions(UnionSymbol union)
        => union.DeclaredCaseTypes.Select(@case => @case.MetadataContainingType)
            .Where(container => !SymbolEqualityComparer.Default.Equals(container, union))
            .Distinct(SymbolEqualityComparer.Default).OfType<NamedTypeSymbol>();

    private static TypeSymbol? LogicalMemberOwner(Symbol member)
        => member.ContainingType is { } owner && CompanionOwners.TryGetValue(owner, out var union)
            ? union : member.ContainingType;

    private static IEnumerable<Symbol> GetLogicalMembers(TypeSymbol type)
        => type.GetMembers().Concat(type is UnionSymbol { IsUnion: true } union
            ? GetUnionCompanions(union).SelectMany(companion => companion.GetMembers())
                .Where(member => member is not UnionCaseTypeSymbol { IsUnionCase: true })
            : []).Distinct(SymbolEqualityComparer.Default);
}
