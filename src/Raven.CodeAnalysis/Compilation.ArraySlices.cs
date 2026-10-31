namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal IMethodSymbol? ResolveArraySliceMethod(IArrayTypeSymbol arrayType, ITypeSymbol rangeType)
        => _target.RuntimeContract.ResolveArraySliceMethod(this, arrayType, rangeType);
}
