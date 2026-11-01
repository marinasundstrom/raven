namespace Raven.CodeAnalysis.Targets;

internal sealed record StringSliceMembers(
    IMethodSymbol GetLength,
    IMethodSymbol GetOffsetAndLength,
    IFieldSymbol Offset,
    IFieldSymbol Length,
    IMethodSymbol Substring);

internal abstract partial class CliRuntimeContract
{
    // Native string representation and slicing support must supply their own contract.
    internal virtual StringSliceMembers? ResolveStringSliceMembers(Compilation compilation, ITypeSymbol rangeType)
        => null;
}
