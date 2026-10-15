using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private Conversion ClassifyStructuralCompositionConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Adding reference nullability is safe; dropping it cannot prove membership.
        if (destination is NullableTypeSymbol nullableDestination)
            destination = nullableDestination.UnderlyingType;

        if (source is LiteralTypeSymbol literal)
            source = literal.UnderlyingType;

        if (source is IStructuralUnionTypeSymbol sourceUnion && destination.IsValueType &&
            sourceUnion.ConstituentTypes.Any(alternative => SymbolEqualityComparer.Default.Equals(destination, alternative)))
            return new Conversion(isImplicit: false, isUnboxing: true);

        if (source.IsValueType && source is not IStructuralUnionTypeSymbol && destination is IStructuralUnionTypeSymbol valueUnion &&
            valueUnion.ConstituentTypes.Any(alternative => SymbolEqualityComparer.Default.Equals(source, alternative)))
            return new Conversion(isImplicit: true, isBoxing: true);

        if (!IsSupportedAlternative(source) || !IsSupportedAlternative(destination))
            return Conversion.None;

        if (HasMembership(source, destination))
            return new Conversion(isImplicit: true, isReference: true);

        // Extracting one exact reference alternative needs a runtime membership
        // check, just as extracting an exact value alternative needs unboxing.
        if (source is IStructuralUnionTypeSymbol referenceUnion && destination.IsReferenceType &&
            referenceUnion.ConstituentTypes.Any(alternative => SymbolEqualityComparer.Default.Equals(destination, alternative)))
            return new Conversion(isImplicit: false, isReference: true);

        return Conversion.None;

        bool HasMembership(ITypeSymbol from, ITypeSymbol to)
        {
            if (SymbolEqualityComparer.Default.Equals(from, to))
                return true;

            if (from is IStructuralUnionTypeSymbol unionSource)
                return unionSource.ConstituentTypes.All(bound => HasMembership(bound, to));

            if (to is IIntersectionTypeSymbol target)
                return target.ConstituentTypes.All(bound => HasMembership(from, bound));

            if (to is IStructuralUnionTypeSymbol unionTarget)
                return unionTarget.ConstituentTypes.Any(bound => HasMembership(from, bound));

            if (from is IIntersectionTypeSymbol origin)
                return origin.ConstituentTypes.Any(bound => HasMembership(bound, to));

            // Do not use general conversion classification here: boxing and
            // user-defined conversions need not preserve the same reference.
            return from.IsReferenceType && (to.SpecialType == SpecialType.System_Object || IsReferenceConversion(from, to));
        }

        static bool IsSupportedAlternative(ITypeSymbol type)
        {
            if (type is IStructuralUnionTypeSymbol union)
                return union.ConstituentTypes.All(IsSupportedAlternative);

            if (type is IIntersectionTypeSymbol intersection)
                return intersection.ConstituentTypes.All(IsSupportedAlternative);

            // Type-parameter entailment and nullable constituent algebra are
            // separate slices. Ordinary nullable wrappers use existing lifting.
            return type is INamedTypeSymbol &&
                (type.IsReferenceType || type.IsValueType) && !type.ContainsErrorType();
        }
    }
}
