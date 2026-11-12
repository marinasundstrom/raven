using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

// Optional declaration-owned projection facts; independent of source binding/storage.
internal interface UnionCaseTypeParameterMapping
{
    ImmutableArray<TypeParameterSymbol> TypeParameters { get; }

    bool TryGetProjectedUnionTypeParameter(TypeParameterSymbol caseTypeParameter,
        [NotNullWhen(true)] out TypeParameterSymbol? unionTypeParameter);
}
