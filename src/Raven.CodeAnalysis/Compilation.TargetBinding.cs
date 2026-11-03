namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal bool SupportsCovariantMethodReturns => _target.RuntimeContract.SupportsCovariantMethodReturns(this);

    internal bool SupportsFunctionTypeMethods => _target.RuntimeContract.SupportsFunctionTypeMethods;
    internal bool SupportsFunctionReturnTypeProjection => _target.RuntimeContract.SupportsFunctionReturnTypeProjection;
    internal bool SupportsIntersectionTypes => _target.RuntimeContract.SupportsIntersectionTypes;
    internal bool SupportsCharKeyword => _target.RuntimeContract.SupportsCharKeyword;

    internal (TypeSymbol Type, NamedTypeSymbol? Definition) NormalizeResolvedTypeForTarget(
        TypeSymbol type, NamedTypeSymbol? definition)
        => _target.RuntimeContract.NormalizeResolvedType(this, type, definition);

    internal NamedTypeSymbol? GetNullableValueMemberLookupType(TypeSymbol type, string memberName)
        => _target.RuntimeContract.GetNullableValueMemberLookupType(this, type, memberName);

    internal TypeSymbol GetPointerElementTypeForTarget(TypeSymbol type)
        => _target.RuntimeContract.GetPointerElementType(this, type);

    internal NamedTypeSymbol? GetSourceMemberLookupType(NamedTypeSymbol type)
        => _target.RuntimeContract.GetSourceMemberLookupType(this, type);
}
