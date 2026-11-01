namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal Targets.StringSliceMembers? ResolveStringSliceMembers(ITypeSymbol rangeType)
        => _target.RuntimeContract.ResolveStringSliceMembers(this, rangeType);

    internal IMethodSymbol? ResolveArraySliceMethod(IArrayTypeSymbol arrayType, ITypeSymbol rangeType)
        => _target.RuntimeContract.ResolveArraySliceMethod(this, arrayType, rangeType);
}
