using Raven.CodeAnalysis;

public static partial class DocumentationGenerator
{
    private static readonly Dictionary<ITypeSymbol, IUnionSymbol> CompanionOwners = new(SymbolEqualityComparer.Default);

    private static IEnumerable<INamedTypeSymbol> GetUnionCompanions(IUnionSymbol union)
        => union.DeclaredCaseTypes.Select(@case => @case.MetadataContainingType)
            .Where(container => !SymbolEqualityComparer.Default.Equals(container, union))
            .Distinct(SymbolEqualityComparer.Default).OfType<INamedTypeSymbol>();

    private static ITypeSymbol? LogicalMemberOwner(ISymbol member)
        => member.ContainingType is { } owner && CompanionOwners.TryGetValue(owner, out var union)
            ? union : member.ContainingType;

    private static IEnumerable<ISymbol> GetLogicalMembers(ITypeSymbol type)
        => type.GetMembers().Concat(type is IUnionSymbol { IsUnion: true } union
            ? GetUnionCompanions(union).SelectMany(companion => companion.GetMembers())
                .Where(member => member is not IUnionCaseTypeSymbol { IsUnionCase: true })
            : []).Distinct(SymbolEqualityComparer.Default);
}
