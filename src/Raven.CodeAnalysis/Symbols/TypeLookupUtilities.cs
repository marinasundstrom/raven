using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal static class TypeLookupUtilities
{
    public static TypeSymbol? SelectBestTypeByName(IEnumerable<TypeSymbol> candidates)
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<Symbol>(SymbolEqualityComparer.Default);

        foreach (var candidate in candidates.OfType<NamedTypeSymbol>())
        {
            if (seen.Add(candidate))
                builder.Add(candidate);
        }

        var namedCandidates = builder.ToImmutable();

        if (namedCandidates.Length == 0)
            return null;

        if (namedCandidates.Length == 1)
            return namedCandidates[0];

        if (namedCandidates.Select(static candidate => candidate.Arity).Distinct().Count() == 1)
            return SelectBestNamedTypeCandidate(namedCandidates);

        var zeroArity = namedCandidates.Where(static candidate => candidate.Arity == 0).ToImmutableArray();
        return SelectBestNamedTypeCandidate(zeroArity);
    }

    public static NamedTypeSymbol? SelectBestNamedTypeByArity(IEnumerable<NamedTypeSymbol> candidates, int arity)
    {
        return SelectBestNamedTypeCandidate(candidates.Where(candidate => candidate.Arity == arity));
    }

    private static NamedTypeSymbol? SelectBestNamedTypeCandidate(IEnumerable<NamedTypeSymbol> candidates)
    {
        var namedCandidates = candidates.ToImmutableArray();

        if (namedCandidates.Length == 0)
            return null;

        if (namedCandidates.Length == 1)
            return namedCandidates[0];

        var sourceCandidates = namedCandidates
            .Where(static candidate => candidate is SourceNamedTypeSymbol)
            .ToImmutableArray();

        return sourceCandidates.Length == 1
            ? sourceCandidates[0]
            : null;
    }
}
