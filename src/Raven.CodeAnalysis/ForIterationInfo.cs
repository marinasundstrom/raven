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
    IMethodSymbol? GetEnumeratorMethod = null,
    IMethodSymbol? MoveNextMethod = null,
    IMethodSymbol? CurrentGetter = null,
    BoundRangeExpression? Range = null)
{
    public static ForIterationInfo ForArray(IArrayTypeSymbol arrayType) =>
        new(ForIterationKind.Array, arrayType.ElementType, arrayType);

    public static ForIterationInfo ForGeneric(
        INamedTypeSymbol enumerableInterface,
        INamedTypeSymbol enumeratorInterface,
        IMethodSymbol getEnumeratorMethod,
        IMethodSymbol moveNextMethod,
        IMethodSymbol currentGetter) =>
        new(ForIterationKind.Generic,
            enumerableInterface.TypeArguments[0],
            null,
            enumerableInterface,
            enumeratorInterface,
            getEnumeratorMethod,
            moveNextMethod,
            currentGetter);

    public static ForIterationInfo ForNonGeneric(
        ITypeSymbol elementType,
        IMethodSymbol? getEnumeratorMethod = null,
        IMethodSymbol? moveNextMethod = null,
        IMethodSymbol? currentGetter = null) =>
        new(ForIterationKind.NonGeneric, elementType, null, null, null, getEnumeratorMethod, moveNextMethod, currentGetter);

    public static ForIterationInfo ForRange(Compilation compilation, BoundRangeExpression range) =>
        new(ForIterationKind.Range, compilation.GetSpecialType(SpecialType.System_Int32), Range: range);
}
