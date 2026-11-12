namespace Raven.CodeAnalysis;

// Canonical type identities used when factories synthesize bound nodes.
// Compilation supplies these from the selected target and runtime contract.
internal interface BoundNodeTypeProvider
{
    NamedTypeSymbol UnitTypeSymbol { get; }

    TypeSymbol NullTypeSymbol { get; }

    TypeSymbol ErrorTypeSymbol { get; }
}
