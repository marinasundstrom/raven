using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class RuntimeSelfTypes
{
    internal static bool IsSelf(Compilation compilation, ITypeSymbol type)
        => compilation.Options.RuntimeSelfTypeContract is { } contract
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
