namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal Targets.RuntimeRangeSliceMembers? ResolveStringSliceMembers(ITypeSymbol rangeType)
        => _target.RuntimeContract.ResolveStringSliceMembers(this, rangeType);

    internal Targets.RuntimeRangeSliceMembers? ResolveSpanSliceMembers(ITypeSymbol receiverType, ITypeSymbol rangeType)
        => _target.RuntimeContract.ResolveSpanSliceMembers(this, receiverType, rangeType);

    internal IMethodSymbol? ResolveArraySliceMethod(IArrayTypeSymbol arrayType, ITypeSymbol rangeType)
        => _target.RuntimeContract.ResolveArraySliceMethod(this, arrayType, rangeType);
}
