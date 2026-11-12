namespace Raven.CodeAnalysis;

// Compilation owns target-aware conversions and array construction; the bound
// function owns delegate compatibility and type-parameter substitution.
internal interface FunctionExpressionServices
{
    Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined);

    TypeSymbol CreateArrayTypeSymbol(TypeSymbol elementType, int rank, int? fixedLength);
}
