namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal Targets.RuntimeRangeSliceMembers? ResolveStringSliceMembers(TypeSymbol rangeType)
        => _target.RuntimeContract.ResolveStringSliceMembers(this, rangeType);

    internal Targets.RuntimeRangeSliceMembers? ResolveSpanSliceMembers(TypeSymbol receiverType, TypeSymbol rangeType)
        => _target.RuntimeContract.ResolveSpanSliceMembers(this, receiverType, rangeType);

    internal MethodSymbol? ResolveArraySliceMethod(ArrayTypeSymbol arrayType, TypeSymbol rangeType)
        => _target.RuntimeContract.ResolveArraySliceMethod(this, arrayType, rangeType);
}
