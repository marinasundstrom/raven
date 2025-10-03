using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal enum ForIterationKind
{
    Array,
    Generic,
    NonGeneric,
}

internal sealed record ForIterationInfo(
    ForIterationKind Kind,
    ITypeSymbol ElementType,
    IArrayTypeSymbol? ArrayType = null,
    INamedTypeSymbol? EnumerableInterface = null,
    INamedTypeSymbol? EnumeratorInterface = null)
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
}
