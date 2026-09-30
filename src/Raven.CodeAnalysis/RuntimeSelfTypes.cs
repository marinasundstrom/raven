using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class RuntimeSelfTypes
{
    internal static INamedTypeSymbol ConformanceOwner(INamedTypeSymbol type, INamedTypeSymbol contract)
    {
        for (var current = type; current is not null; current = current.BaseType)
            if (current.Interfaces.Any(i => i.MetadataIdentityEquals(contract)
                || i.AllInterfaces.Any(inherited => inherited.MetadataIdentityEquals(contract))))
                return current;
        return type;
    }

    internal static bool SatisfiesConstraint(Compilation compilation, ITypeSymbol argument, ITypeSymbol constraint)
    {
        if (!SemanticFacts.SatisfiesTypeConstraint(argument, constraint))
            return false;
        if (!compilation.HasNativeSelfContract
            || constraint is not INamedTypeSymbol { TypeKind: TypeKind.Interface } contract)
            return true;
        if (!HasSelfContract(compilation, contract))
            return true;
        if (argument is ITypeParameterSymbol parameter)
            return parameter.ConstraintTypes.Any(bound => bound is INamedTypeSymbol { TypeKind: TypeKind.Interface } i
                && (i.MetadataIdentityEquals(contract) || i.AllInterfaces.Any(parent => parent.MetadataIdentityEquals(contract))));
        return argument is INamedTypeSymbol { TypeKind: not TypeKind.Interface } named
            && ConformanceOwner(named, contract).MetadataIdentityEquals(named);
    }

    internal static bool HasSelfContract(Compilation compilation, INamedTypeSymbol contract)
        => compilation.HasNativeSelfContract
            && contract.AllInterfaces.Prepend(contract).Any(i => Contains(compilation, i) || i.GetMembers().Any(member => member switch
            {
                IMethodSymbol method => Contains(compilation, method),
                IPropertySymbol property => Contains(compilation, property.Type),
                _ => false
            }));

    internal static bool IsSelf(Compilation compilation, ITypeSymbol type)
        => compilation.HasNativeSelfContract && compilation.Options.RuntimeSelfTypeContract is { } contract
            && type.ContainingAssembly?.Name == contract.AssemblyName
            && type.ToFullyQualifiedMetadataName() == contract.TypeName;

    internal static ITypeSymbol Substitute(Compilation compilation, ITypeSymbol type, ITypeSymbol implementingType)
    {
        if (!Contains(compilation, type))
            return type;
        if (IsSelf(compilation, type))
            return compilation.SelfImplementingType(implementingType);
        if (type is IArrayTypeSymbol array)
            return compilation.CreateArrayTypeSymbol(Substitute(compilation, array.ElementType, implementingType), array.Rank);
        if (type is INamedTypeSymbol named && !named.TypeArguments.IsDefaultOrEmpty)
            return ((INamedTypeSymbol)named.ConstructedFrom!).Construct(named.TypeArguments.Select(t => Substitute(compilation, t, implementingType)).ToArray());
        return type;
    }

    internal static bool Contains(Compilation compilation, ITypeSymbol type)
        => IsSelf(compilation, type)
            || type is IArrayTypeSymbol array && Contains(compilation, array.ElementType)
            || type is INamedTypeSymbol named && named.TypeArguments.Any(t => Contains(compilation, t));

    internal static bool Contains(Compilation compilation, IMethodSymbol method)
        => Contains(compilation, method.ReturnType) || method.Parameters.Any(p => Contains(compilation, p.Type));
}
