using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal enum ForIterationKind
{
    Array,
    Generic,
    Range,
    NonGeneric,
}

internal sealed record ForIterationInfo(
    ForIterationKind Kind,
    ITypeSymbol ElementType,
    IArrayTypeSymbol? ArrayType = null,
    INamedTypeSymbol? EnumerableInterface = null,
    INamedTypeSymbol? EnumeratorInterface = null,
    BoundRangeExpression? Range = null)
{
    public static ForIterationInfo ForArray(IArrayTypeSymbol arrayType) =>
        new(ForIterationKind.Array, arrayType.ElementType, arrayType);

    public static ForIterationInfo ForGeneric(
        INamedTypeSymbol enumerableInterface,
        INamedTypeSymbol enumeratorInterface) =>
        new(ForIterationKind.Generic,
            enumerableInterface.TypeArguments[0],
            null,
            enumerableInterface,
            enumeratorInterface);

    public static ForIterationInfo ForNonGeneric(ITypeSymbol elementType) =>
        new(ForIterationKind.NonGeneric, elementType);

    public static ForIterationInfo ForRange(Compilation compilation, BoundRangeExpression range) =>
        new(ForIterationKind.Range, compilation.GetSpecialType(SpecialType.System_Int32), Range: range);
}
