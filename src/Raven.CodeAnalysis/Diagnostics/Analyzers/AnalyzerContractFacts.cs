using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Diagnostics;

internal static class AnalyzerContractFacts
{
    public static bool IsContractMethod(MethodSymbol method)
    {
        method = UnwrapMethod(method);

        return method.MethodKind == MethodKind.ExplicitInterfaceImplementation ||
               method.IsVirtual ||
               method.IsOverride ||
               HasOverriddenMethod(method) ||
               !method.ExplicitInterfaceImplementations.IsDefaultOrEmpty ||
               ImplementsInterfaceMethod(method);
    }

    public static bool IsContractProperty(PropertySymbol property)
    {
        property = UnwrapProperty(property);

        return !property.ExplicitInterfaceImplementations.IsDefaultOrEmpty ||
               IsContractAccessor(property.GetMethod) ||
               IsContractAccessor(property.SetMethod) ||
               ImplementsInterfaceProperty(property);
    }

    private static MethodSymbol UnwrapMethod(MethodSymbol method)
        => method.UnderlyingSymbol as MethodSymbol ?? method;

    private static PropertySymbol UnwrapProperty(PropertySymbol property)
        => property.UnderlyingSymbol as PropertySymbol ?? property;

    private static bool IsContractAccessor(MethodSymbol? accessor)
    {
        if (accessor is null)
            return false;

        accessor = UnwrapMethod(accessor);
        return accessor.IsVirtual || accessor.IsOverride || HasOverriddenMethod(accessor);
    }

    private static bool HasOverriddenMethod(MethodSymbol method)
        => method is SourceMethodSymbol { OverriddenMethod: not null };

    private static bool ImplementsInterfaceMethod(MethodSymbol method)
    {
        if (method.ContainingType is null)
            return false;

        foreach (var interfaceType in method.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(method.Name).OfType<MethodSymbol>())
            {
                if (interfaceMember.MethodKind != MethodKind.Ordinary)
                    continue;

                if (HasSameSignature(method, interfaceMember))
                    return true;
            }
        }

        return false;
    }

    private static bool ImplementsInterfaceProperty(PropertySymbol property)
    {
        if (property.ContainingType is null)
            return false;

        foreach (var interfaceType in property.ContainingType.AllInterfaces)
        {
            foreach (var interfaceMember in interfaceType.GetMembers(property.Name).OfType<PropertySymbol>())
            {
                if (interfaceMember.IsIndexer != property.IsIndexer)
                    continue;

                if (SymbolEqualityComparer.Default.Equals(property.Type, interfaceMember.Type))
                    return true;
            }
        }

        return false;
    }

    private static bool HasSameSignature(MethodSymbol candidate, MethodSymbol contract)
    {
        if (!SymbolEqualityComparer.Default.Equals(candidate.ReturnType, contract.ReturnType))
            return false;

        return HasSameParameters(candidate.Parameters, contract.Parameters);
    }

    private static bool HasSameParameters(
        ImmutableArray<ParameterSymbol> left,
        ImmutableArray<ParameterSymbol> right)
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
