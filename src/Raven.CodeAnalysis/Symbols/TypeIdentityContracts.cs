using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

// Internal identity facts, independent of symbol storage and binding services.
// These are provider contracts, not additional public symbol categories.
internal interface INullableTypeIdentity
{
    TypeSymbol UnderlyingType { get; }
}

internal interface IStructuralTupleIdentity
{
    ImmutableArray<FieldSymbol> TupleElements { get; }
}

internal interface IStructuralFunctionIdentity
{
    ImmutableArray<TypeSymbol> ParameterTypes { get; }
    ImmutableArray<RefKind> ParameterRefKinds { get; }
    TypeSymbol ReturnType { get; }
}

internal interface IUnitTypeIdentity
{
    // The target-selected representation; null leaves semantic Unit unchanged.
    NamedTypeSymbol? RuntimeRepresentation { get; }
}
