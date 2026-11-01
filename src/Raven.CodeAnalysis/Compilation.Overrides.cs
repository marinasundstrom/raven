namespace Raven.CodeAnalysis;

public partial class Compilation
{
    internal bool IsCovariantMethodReturn(ITypeSymbol baseReturn, ITypeSymbol derivedReturn)
    {
        if (!baseReturn.IsReferenceType || !derivedReturn.IsReferenceType ||
            (!baseReturn.IsNullable && derivedReturn.IsNullable))
            return false;
        var conversion = ClassifyConversion(derivedReturn.GetNonNullableType(), baseReturn.GetNonNullableType());
        return conversion.IsImplicit && conversion.IsReference && !conversion.IsIdentity;
    }
}
