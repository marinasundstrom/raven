namespace Raven.CodeAnalysis;

// Canonical type identities needed by built-in operator lookup.
// Compilation supplies these from the selected target and runtime contract.
internal interface OperatorTypeProvider
{
    TypeSymbol ErrorTypeSymbol { get; }

    NamedTypeSymbol GetSpecialType(SpecialType specialType);
}
