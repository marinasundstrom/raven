using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal enum ForIterationKind
{
    Array,
    Generic,
    Async,
    Range,
    NonGeneric,
}

internal sealed record ForIterationInfo(
    ForIterationKind Kind,
    TypeSymbol ElementType,
    ArrayTypeSymbol? ArrayType = null,
    NamedTypeSymbol? EnumerableInterface = null,
    NamedTypeSymbol? EnumeratorInterface = null,
    MethodSymbol? GetEnumeratorMethod = null,
    MethodSymbol? MoveNextMethod = null,
    MethodSymbol? MoveNextAsyncMethod = null,
    MethodSymbol? CurrentGetter = null,
    MethodSymbol? DisposeAsyncMethod = null,
    BoundRangeExpression? Range = null,
    BoundExpression? RangeStart = null,
    BoundExpression? RangeEnd = null,
    BoundExpression? RangeStep = null,
    bool RangeUpperExclusive = false,
    BoundExpression? CancellationToken = null)
{
    public static ForIterationInfo ForArray(ArrayTypeSymbol arrayType) =>
        new(ForIterationKind.Array, arrayType.ElementType, arrayType);

    public static ForIterationInfo ForGeneric(
        NamedTypeSymbol enumerableInterface,
        NamedTypeSymbol enumeratorInterface,
        MethodSymbol getEnumeratorMethod,
        MethodSymbol moveNextMethod,
        MethodSymbol currentGetter) =>
        new(ForIterationKind.Generic,
            enumerableInterface.TypeArguments[0],
            null,
            enumerableInterface,
            enumeratorInterface,
            getEnumeratorMethod,
            moveNextMethod,
            null,
            currentGetter);

    public static ForIterationInfo ForAsync(
        TypeSymbol elementType,
        MethodSymbol getAsyncEnumeratorMethod,
        MethodSymbol moveNextAsyncMethod,
        MethodSymbol currentGetter,
        MethodSymbol? disposeAsyncMethod = null) =>
        new(ForIterationKind.Async,
            elementType,
            null,
            null,
            null,
            getAsyncEnumeratorMethod,
            null,
            moveNextAsyncMethod,
            currentGetter,
            disposeAsyncMethod);

    public static ForIterationInfo ForNonGeneric(
        TypeSymbol elementType,
        MethodSymbol? getEnumeratorMethod = null,
        MethodSymbol? moveNextMethod = null,
        MethodSymbol? currentGetter = null) =>
        new(ForIterationKind.NonGeneric, elementType, null, null, null, getEnumeratorMethod, moveNextMethod, null, currentGetter);

    public static ForIterationInfo ForRange(
        TypeSymbol elementType,
        BoundExpression start,
        BoundExpression end,
        BoundExpression step,
        BoundRangeExpression? range = null,
        bool rangeUpperExclusive = false) =>
        new(
            ForIterationKind.Range,
            elementType,
            Range: range,
            RangeStart: start,
            RangeEnd: end,
            RangeStep: step,
            RangeUpperExclusive: rangeUpperExclusive);
}
