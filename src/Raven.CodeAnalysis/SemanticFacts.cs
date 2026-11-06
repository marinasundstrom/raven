using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public static class SemanticFacts
{
    public static bool IsDerivedFrom(
        this TypeSymbol? type,
        TypeSymbol? potentialBase,
        SymbolEqualityComparer? comparer = null)
    {
        if (type is null || potentialBase is null)
            return false;

        if (type is TypeParameterSymbol typeParameter)
            return IsDerivedFromTypeParameter(typeParameter, potentialBase, CreateVisitedSet(comparer ?? SymbolEqualityComparer.Default));

        return HasBaseType(type, potentialBase);
    }

    private static bool HasBaseType(TypeSymbol type, TypeSymbol potentialBase)
    {
        HashSet<Symbol>? visited = null;
        for (var current = type.BaseType; current is not null && !ReferenceEquals(current, type); current = current.BaseType)
        {
            if (current.MetadataIdentityEquals(potentialBase))
                return true;
            if (!(visited ??= new(ReferenceEqualityComparer.Instance)).Add(current))
                break;
        }

        return false;
    }

    public static bool ImplementsInterface(
        this TypeSymbol? type,
        NamedTypeSymbol? interfaceType,
        SymbolEqualityComparer? comparer = null)
    {
        if (type is null || interfaceType is null)
            return false;

        if (interfaceType.TypeKind != TypeKind.Interface)
            throw new ArgumentException("interfaceType must be an interface symbol.", nameof(interfaceType));

        return ImplementsInterfaceCore(type, interfaceType, comparer ?? SymbolEqualityComparer.Default, visited: null);
    }

    private static bool ImplementsInterfaceCore(
        TypeSymbol type, NamedTypeSymbol interfaceType, SymbolEqualityComparer comparer, HashSet<Symbol>? visited)
    {
        if (type.MetadataIdentityEquals(interfaceType))
            return true;

        if (type is NamedTypeSymbol typeInterface &&
            typeInterface.TypeKind == TypeKind.Interface &&
            IsVariantCompatible(typeInterface, interfaceType, comparer))
        {
            return true;
        }

        if (type is TypeParameterSymbol typeParameter)
        {
            visited ??= CreateVisitedSet(comparer);
            if (!visited.Add(typeParameter))
                return false;
            foreach (var bound in typeParameter.ConstraintTypes)
            {
                var constraint = UnwrapReferenceAnnotation(bound);
                if (ImplementsInterfaceCore(constraint, interfaceType, comparer, visited))
                    return true;
            }
            return false;
        }

        foreach (var implementedInterface in type.AllInterfaces)
        {
            if (implementedInterface.MetadataIdentityEquals(interfaceType))
                return true;

            if (IsVariantCompatible(implementedInterface, interfaceType, comparer))
                return true;
        }

        return false;
    }

    internal static bool SatisfiesFunctionTypeConstraint(TypeSymbol type) =>
        type is IStructuralFunctionIdentity ||
        type is TypeParameterSymbol parameter && parameter.ConstraintKind.HasFlag(TypeParameterConstraintKind.FunctionType);

    public static bool SatisfiesConstraints(
        this TypeSymbol? typeArgument,
        TypeParameterSymbol? typeParameter)
    {
        if (typeArgument is null || typeParameter is null)
            return false;

        if (typeArgument is ErrorTypeSymbol)
            return true;

        var constraintKind = typeParameter.ConstraintKind;

        if (MayBeRefLike(typeArgument) &&
            (constraintKind & TypeParameterConstraintKind.AllowByRefLike) == 0)
        {
            return false;
        }

        if (constraintKind.HasFlag(TypeParameterConstraintKind.FunctionType) && !SatisfiesFunctionTypeConstraint(typeArgument))
            return false;

        if ((constraintKind & TypeParameterConstraintKind.ReferenceType) != 0 &&
            !SatisfiesReferenceTypeConstraint(typeArgument))
        {
            return false;
        }

        if ((constraintKind & TypeParameterConstraintKind.ValueType) != 0 &&
            !SatisfiesValueTypeConstraint(typeArgument))
        {
            return false;
        }

        if ((constraintKind & TypeParameterConstraintKind.NotNull) != 0 &&
            !SatisfiesNotNullConstraint(typeArgument))
        {
            return false;
        }

        if ((constraintKind & TypeParameterConstraintKind.Constructor) != 0 &&
            !SatisfiesConstructorConstraint(typeArgument))
        {
            return false;
        }

        if ((constraintKind & TypeParameterConstraintKind.TypeConstraint) != 0)
        {
            foreach (var constraintType in typeParameter.ConstraintTypes)
            {
                if (constraintType is ErrorTypeSymbol)
                    continue;

                if (!SatisfiesTypeConstraint(typeArgument, constraintType))
                    return false;
            }
        }

        return true;
    }

    public static bool MayBeRefLike(TypeSymbol type)
        => type is NamedTypeSymbol { IsRefLikeType: true } ||
           type is TypeParameterSymbol typeParameter &&
           (typeParameter.ConstraintKind & TypeParameterConstraintKind.AllowByRefLike) != 0;

    public static bool SatisfiesReferenceTypeConstraint(TypeSymbol type)
    {
        if (type.IsReferenceType)
            return true;

        if (type is TypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;

        return false;
    }

    public static bool SatisfiesValueTypeConstraint(TypeSymbol type)
    {
        if (type.TypeKind == TypeKind.Nullable)
            return false;

        if (type.IsValueType)
            return true;

        if (type is TypeParameterSymbol typeParameter)
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;

        return false;
    }

    public static bool SatisfiesNotNullConstraint(TypeSymbol type)
    {
        if (type.IsNullable)
            return false;

        if (type is TypeParameterSymbol typeParameter)
        {
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.NotNull) != 0
                || (typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0
                || (typeParameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0;
        }

        return true;
    }

    public static bool SatisfiesConstructorConstraint(TypeSymbol type)
    {
        if (type.IsValueType)
            return true;

        if (type is TypeParameterSymbol typeParameter)
        {
            return (typeParameter.ConstraintKind & TypeParameterConstraintKind.Constructor) != 0
                || (typeParameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;
        }

        if (type is NamedTypeSymbol namedType)
        {
            if (namedType.IsAbstract)
                return false;

            foreach (var constructor in namedType.InstanceConstructors)
            {
                if (constructor.Parameters.Length == 0 &&
                    constructor.DeclaredAccessibility == Accessibility.Public)
                {
                    return true;
                }
            }

            return false;
        }

        return false;
    }

    /// <summary>Checks type membership against nominal or structural generic bounds without value conversions.</summary>
    /// <remarks>Every alternative of a structural union argument must satisfy the bound. Union bounds are
    /// disjunctive and intersection bounds are conjunctive. Recursive structural proofs are bounded to 64 steps.</remarks>
    public static bool SatisfiesTypeConstraint(TypeSymbol typeArgument, TypeSymbol constraintType)
        => SatisfiesTypeConstraintCore(typeArgument, constraintType, 0);

    private static bool SatisfiesTypeConstraintCore(TypeSymbol typeArgument, TypeSymbol constraintType, int depth)
    {
        if (depth > 64)
            return false;
        bool Satisfies(TypeSymbol argument, TypeSymbol bound) => SatisfiesTypeConstraintCore(argument, bound, depth + 1);
        // A union argument must be entirely covered, not merely overlap the bound.
        if (typeArgument is StructuralUnionTypeSymbol sourceUnion)
            return sourceUnion.ConstituentTypes.All(t => Satisfies(t, constraintType));
        if (constraintType is IntersectionTypeSymbol intersection)
            return intersection.ConstituentTypes.All(t => Satisfies(typeArgument, t));
        if (typeArgument is TypeParameterSymbol parameter &&
            (constraintType is StructuralUnionTypeSymbol || parameter.ConstraintTypes.Any(t => t is StructuralUnionTypeSymbol or IntersectionTypeSymbol)))
            return parameter.ConstraintTypes.Any(t => Satisfies(t, constraintType));
        if (constraintType is StructuralUnionTypeSymbol union)
            return union.ConstituentTypes.Any(t => Satisfies(typeArgument, t));
        if (typeArgument is IntersectionTypeSymbol sourceIntersection)
            return sourceIntersection.ConstituentTypes.Any(t => Satisfies(t, constraintType));
        if (typeArgument is ErrorTypeSymbol || constraintType is ErrorTypeSymbol)
            return true;

        // Reference nullability does not change nominal CLR type membership.
        // Keep nullable value types intact: they have distinct runtime storage.
        typeArgument = UnwrapReferenceAnnotation(typeArgument);
        constraintType = UnwrapReferenceAnnotation(constraintType);
        return constraintType is not NamedTypeSymbol namedConstraint ||
            SatisfiesNamedTypeConstraint(typeArgument, namedConstraint);
    }

    public static bool SatisfiesNamedTypeConstraint(TypeSymbol typeArgument, NamedTypeSymbol constraintType)
    {
        if (constraintType.TypeKind == TypeKind.Interface)
            return ImplementsInterface(typeArgument, constraintType);

        return typeArgument.MetadataIdentityEquals(constraintType) || IsDerivedFrom(typeArgument, constraintType);
    }

    private static TypeSymbol UnwrapReferenceAnnotation(TypeSymbol type)
        => type is INullableTypeIdentity { UnderlyingType.IsReferenceType: true } nullable
            ? nullable.UnderlyingType : type;

    private static bool IsDerivedFromTypeParameter(
        TypeParameterSymbol typeParameter,
        TypeSymbol potentialBase,
        HashSet<Symbol> visited)
    {
        if (!visited.Add(typeParameter))
            return false;

        potentialBase = UnwrapReferenceAnnotation(potentialBase);

        foreach (var declaredConstraint in typeParameter.ConstraintTypes)
        {
            // Reference annotations do not remove nominal base/interface bounds.
            // Nullable value types retain their distinct runtime identity.
            var constraint = UnwrapReferenceAnnotation(declaredConstraint);
            if (constraint.MetadataIdentityEquals(potentialBase))
                return true;

            if (constraint is TypeParameterSymbol nested)
            {
                if (IsDerivedFromTypeParameter(nested, potentialBase, visited))
                    return true;
            }
            else if (HasBaseType(constraint, potentialBase))
            {
                return true;
            }
        }

        return false;
    }

    private static HashSet<Symbol> CreateVisitedSet(SymbolEqualityComparer comparer)
        => new(SymbolEqualityComparerAdapter.Get(comparer));

    private static bool IsVariantCompatible(
        NamedTypeSymbol implementedInterface,
        NamedTypeSymbol targetInterface,
        SymbolEqualityComparer comparer)
    {
        if (!implementedInterface.IsGenericType || !targetInterface.IsGenericType)
            return false;

        if (implementedInterface.ConstructedFrom is not NamedTypeSymbol implementedDefinition ||
            targetInterface.ConstructedFrom is not NamedTypeSymbol targetDefinition)
        {
            return false;
        }

        if (!implementedDefinition.MetadataIdentityEquals(targetDefinition))
            return false;

        var typeParameters = implementedDefinition.TypeParameters;
        var implementedArguments = implementedInterface.TypeArguments;
        var targetArguments = targetInterface.TypeArguments;

        if (typeParameters.Length != implementedArguments.Length ||
            typeParameters.Length != targetArguments.Length)
        {
            return false;
        }

        for (var i = 0; i < typeParameters.Length; i++)
        {
            var parameter = typeParameters[i];
            var implementedArgument = implementedArguments[i];
            var targetArgument = targetArguments[i];

            switch (parameter.Variance)
            {
                case VarianceKind.Out:
                    if (!AreAssignable(implementedArgument, targetArgument, comparer))
                        return false;
                    break;
                case VarianceKind.In:
                    if (!AreAssignable(targetArgument, implementedArgument, comparer))
                        return false;
                    break;
                default:
                    if (!implementedArgument.MetadataIdentityEquals(targetArgument))
                        return false;
                    break;
            }
        }

        return true;
    }

    private static bool AreAssignable(
        TypeSymbol source,
        TypeSymbol destination,
        SymbolEqualityComparer comparer)
    {
        // Nullable references keep the same runtime representation, but variance
        // must preserve the direction of Raven's nullability contract.
        if (source is INullableTypeIdentity { UnderlyingType.IsReferenceType: true } nullableSource)
        {
            if (destination is not INullableTypeIdentity { UnderlyingType.IsReferenceType: true })
                return false;
            source = nullableSource.UnderlyingType;
        }
        if (destination is INullableTypeIdentity { UnderlyingType.IsReferenceType: true } nullableDestination)
            destination = nullableDestination.UnderlyingType;

        if (source.MetadataIdentityEquals(destination))
            return true;

        // Variance preserves reference representation; boxing and unconstrained
        // type parameters cannot establish a reference conversion.
        if (!source.IsReferenceType || !destination.IsReferenceType)
            return false;

        if (destination.SpecialType == SpecialType.System_Object)
            return true;

        if (IsDerivedFrom(source, destination, comparer))
            return true;

        if (destination is NamedTypeSymbol destinationInterface &&
            destinationInterface.TypeKind == TypeKind.Interface &&
            ImplementsInterface(source, destinationInterface, comparer))
        {
            return true;
        }

        return false;
    }

    private sealed class SymbolEqualityComparerAdapter : IEqualityComparer<Symbol>
    {
        private readonly SymbolEqualityComparer _comparer;

        private SymbolEqualityComparerAdapter(SymbolEqualityComparer comparer)
            => _comparer = comparer;

        public static IEqualityComparer<Symbol> Get(SymbolEqualityComparer comparer)
            => comparer as IEqualityComparer<Symbol> ?? new SymbolEqualityComparerAdapter(comparer);

        public bool Equals(Symbol? x, Symbol? y)
            => _comparer.Equals(x, y);

        public int GetHashCode(Symbol obj)
            => _comparer.GetHashCode(obj);
    }

}
