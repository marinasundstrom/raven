using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private Conversion ClassifyIntersectionReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Adding reference nullability is safe; dropping it cannot prove membership.
        if (destination is NullableTypeSymbol nullableDestination)
            destination = nullableDestination.UnderlyingType;

        if (!IsSupportedReference(source) || !IsSupportedReference(destination))
            return Conversion.None;

        return HasMembership(source, destination)
            ? new Conversion(isImplicit: true, isReference: true)
            : Conversion.None;

        bool HasMembership(ITypeSymbol from, ITypeSymbol to)
        {
            if (SymbolEqualityComparer.Default.Equals(from, to))
                return true;

            if (to is IIntersectionTypeSymbol target)
                return target.ConstituentTypes.All(bound => HasMembership(from, bound));

            if (from is IIntersectionTypeSymbol origin)
                return origin.ConstituentTypes.Any(bound => HasMembership(bound, to));

            // Do not use general conversion classification here: boxing and
            // user-defined conversions need not preserve the same reference.
            return to.SpecialType == SpecialType.System_Object || IsReferenceConversion(from, to);
        }

        static bool IsSupportedReference(ITypeSymbol type)
        {
            if (type is IIntersectionTypeSymbol intersection)
                return intersection.ConstituentTypes.All(IsSupportedReference);

            // Type-parameter entailment and nullable constituent algebra are
            // separate slices. Ordinary nullable wrappers use existing lifting.
            return type is INamedTypeSymbol &&
                type.IsReferenceType && !type.ContainsErrorType();
        }
    }
}
