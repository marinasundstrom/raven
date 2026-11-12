namespace Raven.CodeAnalysis;

// Compiler-owned services needed by binary lookup; lookup owns operator semantics.
internal interface BinaryOperatorServices : OperatorTypeProvider
{
    Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined);

    TypeSymbol CreateNullableType(TypeSymbol type);

    bool HasRuntimeSelfContract { get; }

    MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method);
}
