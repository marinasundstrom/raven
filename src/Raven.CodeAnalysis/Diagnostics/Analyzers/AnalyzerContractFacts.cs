using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Diagnostics;

internal static class AnalyzerContractFacts
{
    public static bool IsContractMethod(IMethodSymbol method)
    {
        method = UnwrapMethod(method);

        return method.MethodKind == MethodKind.ExplicitInterfaceImplementation ||
               method.IsVirtual ||
               method.IsOverride ||
               HasOverriddenMethod(method) ||
               !method.ExplicitInterfaceImplementations.IsDefaultOrEmpty ||
               ImplementsInterfaceMethod(method);
    }

    public static bool IsContractProperty(IPropertySymbol property)
    {
        property = UnwrapProperty(property);

        return !property.ExplicitInterfaceImplementations.IsDefaultOrEmpty ||
               IsContractAccessor(property.GetMethod) ||
               IsContractAccessor(property.SetMethod) ||
               ImplementsInterfaceProperty(property);
    }

    private static IMethodSymbol UnwrapMethod(IMethodSymbol method)
        => method.UnderlyingSymbol as IMethodSymbol ?? method;

    private static IPropertySymbol UnwrapProperty(IPropertySymbol property)
        => property.UnderlyingSymbol as IPropertySymbol ?? property;

    private static bool IsContractAccessor(IMethodSymbol? accessor)
    {
        if (accessor is null)
            return false;

        accessor = UnwrapMethod(accessor);
        return accessor.IsVirtual || accessor.IsOverride || HasOverriddenMethod(accessor);
    }

    private static bool HasOverriddenMethod(IMethodSymbol method)
        => method is SourceMethodSymbol { OverriddenMethod: not null };

    private static bool ImplementsInterfaceMethod(IMethodSymbol method)
    {
        if (method.ContainingType is null)
            return false;

        foreach (var interfaceType in method.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(method.Name).OfType<IMethodSymbol>())
            {
                if (interfaceMember.MethodKind != MethodKind.Ordinary)
                    continue;

                if (HasSameSignature(method, interfaceMember))
                    return true;
            }
        }

        return false;
    }

    private static bool ImplementsInterfaceProperty(IPropertySymbol property)
    {
        if (property.ContainingType is null)
            return false;

        foreach (var interfaceType in property.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(property.Name).OfType<IPropertySymbol>())
            {
                if (interfaceMember.IsIndexer != property.IsIndexer)
                    continue;

                if (SymbolEqualityComparer.Default.Equals(property.Type, interfaceMember.Type))
                    return true;
            }
        }

        return false;
    }

    private static bool HasSameSignature(IMethodSymbol candidate, IMethodSymbol contract)
    {
        if (!SymbolEqualityComparer.Default.Equals(candidate.ReturnType, contract.ReturnType))
            return false;

        return HasSameParameters(candidate.Parameters, contract.Parameters);
    }

    private static bool HasSameParameters(
        ImmutableArray<IParameterSymbol> left,
        ImmutableArray<IParameterSymbol> right)
    {
        if (left.Length != right.Length)
            return false;

        for (var i = 0; i < left.Length; i++)
        {
            if (left[i].RefKind != right[i].RefKind)
                return false;

            if (!SymbolEqualityComparer.Default.Equals(left[i].Type, right[i].Type))
                return false;
        }

        return true;
    }
}
