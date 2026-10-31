namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal bool SupportsFunctionTypeMethods => _target.RuntimeContract.SupportsFunctionTypeMethods;
    internal bool SupportsFunctionReturnTypeProjection => _target.RuntimeContract.SupportsFunctionReturnTypeProjection;
    internal bool SupportsIntersectionTypes => _target.RuntimeContract.SupportsIntersectionTypes;
    internal bool SupportsCharKeyword => _target.RuntimeContract.SupportsCharKeyword;

    internal (ITypeSymbol Type, INamedTypeSymbol? Definition) NormalizeResolvedTypeForTarget(
        ITypeSymbol type, INamedTypeSymbol? definition)
        => _target.RuntimeContract.NormalizeResolvedType(this, type, definition);

    internal ITypeSymbol GetPointerElementTypeForTarget(ITypeSymbol type)
        => _target.RuntimeContract.GetPointerElementType(this, type);

    internal INamedTypeSymbol? GetSourceMemberLookupType(INamedTypeSymbol type)
        => _target.RuntimeContract.GetSourceMemberLookupType(this, type);
}
