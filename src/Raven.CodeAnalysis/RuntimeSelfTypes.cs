using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class RuntimeSelfTypes
{
    internal static NamedTypeSymbol ConformanceOwner(NamedTypeSymbol type, NamedTypeSymbol contract)
    {
        for (var current = type; current is not null; current = current.BaseType)
            if (current.Interfaces.Any(i => i.MetadataIdentityEquals(contract)
                || i.AllInterfaces.Any(inherited => inherited.MetadataIdentityEquals(contract))))
                return current;
        return type;
    }

    internal static bool SatisfiesConstraint(Compilation compilation, TypeSymbol argument, TypeSymbol constraint)
        => SatisfiesConstraintCore(compilation, argument, constraint, 0);

    private static bool SatisfiesConstraintCore(Compilation compilation, TypeSymbol argument, TypeSymbol constraint, int depth)
    {
        if (depth > 64)
            return false;
        bool Satisfies(TypeSymbol type, TypeSymbol bound) => SatisfiesConstraintCore(compilation, type, bound, depth + 1);
        if (argument is StructuralUnionTypeSymbol sourceUnion)
            return sourceUnion.ConstituentTypes.All(t => Satisfies(t, constraint));
        if (constraint is IntersectionTypeSymbol intersection)
            return intersection.ConstituentTypes.All(t => Satisfies(argument, t));
        if (argument is TypeParameterSymbol parameterArgument &&
            (constraint is StructuralUnionTypeSymbol || parameterArgument.ConstraintTypes.Any(t => t is StructuralUnionTypeSymbol or IntersectionTypeSymbol)))
            return parameterArgument.ConstraintTypes.Any(t => Satisfies(t, constraint));
        if (constraint is StructuralUnionTypeSymbol union)
            return union.ConstituentTypes.Any(t => Satisfies(argument, t));
        if (argument is IntersectionTypeSymbol sourceIntersection)
            return sourceIntersection.ConstituentTypes.Any(t => Satisfies(t, constraint));
        if (!SemanticFacts.SatisfiesTypeConstraint(argument, constraint))
            return false;
        if (!compilation.HasNativeSelfContract
            || constraint is not NamedTypeSymbol { TypeKind: TypeKind.Interface } contract)
            return true;
        if (!HasSelfContract(compilation, contract))
            return true;
        if (argument is TypeParameterSymbol parameter)
            return parameter.ConstraintTypes.Any(bound => bound is NamedTypeSymbol { TypeKind: TypeKind.Interface } i
                && (i.MetadataIdentityEquals(contract) || i.AllInterfaces.Any(parent => parent.MetadataIdentityEquals(contract))));
        return argument is NamedTypeSymbol { TypeKind: not TypeKind.Interface } named
            && ConformanceOwner(named, contract).MetadataIdentityEquals(named);
    }

    internal static bool HasSelfContract(Compilation compilation, NamedTypeSymbol contract)
        => compilation.HasNativeSelfContract
            && contract.AllInterfaces.Prepend(contract).Any(i => Contains(compilation, i) || i.GetMembers().Any(member => member switch
            {
                MethodSymbol method => Contains(compilation, method),
                PropertySymbol property => Contains(compilation, property.Type),
                _ => false
            }));

    internal static bool IsSelf(Compilation compilation, TypeSymbol type)
        => compilation.HasNativeSelfContract && compilation.Options.RuntimeSelfTypeContract is { } contract
            && type.ContainingAssembly?.Name == contract.AssemblyName
            && type.ToFullyQualifiedMetadataName() == contract.TypeName;

    internal static TypeSymbol Substitute(Compilation compilation, TypeSymbol type, TypeSymbol implementingType)
    {
        if (!Contains(compilation, type))
            return type;
        if (IsSelf(compilation, type))
            return compilation.SelfImplementingType(implementingType);
        if (type is ArrayTypeSymbol array)
            return compilation.CreateArrayTypeSymbol(Substitute(compilation, array.ElementType, implementingType), array.Rank);
        if (type is NamedTypeSymbol named && !named.TypeArguments.IsDefaultOrEmpty)
            return ((NamedTypeSymbol)named.ConstructedFrom!).Construct(named.TypeArguments.Select(t => Substitute(compilation, t, implementingType)).ToArray());
        return type;
    }

    internal static bool Contains(Compilation compilation, TypeSymbol type)
        => IsSelf(compilation, type)
            || type is ArrayTypeSymbol array && Contains(compilation, array.ElementType)
            || type is NamedTypeSymbol named && named.TypeArguments.Any(t => Contains(compilation, t));

    internal static bool Contains(Compilation compilation, MethodSymbol method)
        => Contains(compilation, method.ReturnType) || method.Parameters.Any(p => Contains(compilation, p.Type));
}
