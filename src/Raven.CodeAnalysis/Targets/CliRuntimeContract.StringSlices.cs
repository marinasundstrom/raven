namespace Raven.CodeAnalysis.Targets;

internal sealed record RuntimeRangeSliceMembers(
    IMethodSymbol GetLength,
    IMethodSymbol GetOffsetAndLength,
    IFieldSymbol Offset,
    IFieldSymbol Length,
    IMethodSymbol Slice);

internal abstract partial class CliRuntimeContract
{
    internal virtual RuntimeRangeSliceMembers? ResolveSpanSliceMembers(Compilation compilation, ITypeSymbol receiverType, ITypeSymbol rangeType)
        => null;

    // Native string representation and slicing support must supply their own contract.
    internal virtual RuntimeRangeSliceMembers? ResolveStringSliceMembers(Compilation compilation, ITypeSymbol rangeType)
        => null;
}
