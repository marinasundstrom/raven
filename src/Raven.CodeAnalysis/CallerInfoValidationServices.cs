using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

// Compilation retains canonical type/conversion ownership and source attribute binding.
internal interface CallerInfoValidationServices
{
    NamedTypeSymbol GetSpecialType(SpecialType specialType);

    Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined);

    ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol);
}
