namespace Raven.CodeAnalysis.Targets;

internal sealed record RuntimeRangeSliceMembers(
    MethodSymbol GetLength,
    MethodSymbol GetOffsetAndLength,
    FieldSymbol Offset,
    FieldSymbol Length,
    MethodSymbol Slice);

internal abstract partial class CliRuntimeContract
{
    internal virtual RuntimeRangeSliceMembers? ResolveSpanSliceMembers(Compilation compilation, TypeSymbol receiverType, TypeSymbol rangeType)
        => null;

    // Native string representation and slicing support must supply their own contract.
    internal virtual RuntimeRangeSliceMembers? ResolveStringSliceMembers(Compilation compilation, TypeSymbol rangeType)
        => null;
}
