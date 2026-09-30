using System.Linq;

namespace Raven.CodeAnalysis.Targets;

internal sealed record RuntimeTypeOfBinding(ITypeSymbol Type, IMethodSymbol CurrentGetter, IMethodSymbol Resolver);

internal abstract partial class CliRuntimeContract
{
    internal RuntimeTypeOfBinding? ResolveTypeOf(Compilation compilation)
    {
        if (Options.RuntimeTypeOfContract is not { } contract
            || string.IsNullOrWhiteSpace(contract.AssemblyName)
            || string.IsNullOrWhiteSpace(contract.TypeInfoTypeName)
            || string.IsNullOrWhiteSpace(contract.ContextTypeName))
            return null;

        var info = compilation.GetTypeByMetadataName(contract.TypeInfoTypeName);
        var context = compilation.GetTypeByMetadataName(contract.ContextTypeName);
        if (info is null || context is null
            || info.ContainingAssembly?.Name != contract.AssemblyName
            || context.ContainingAssembly?.Name != contract.AssemblyName
            || info.TypeKind != TypeKind.Interface || context.TypeKind != TypeKind.Class
            || info.Arity != 0 || context.Arity != 0
            || info.ContainingType is not null || context.ContainingType is not null
            || info.DeclaredAccessibility != Accessibility.Public
            || context.DeclaredAccessibility != Accessibility.Public)
            return null;

        var getters = context.GetMembers("Current").OfType<IPropertySymbol>()
            .Where(p => p.IsStatic && p.Parameters.Length == 0
                && SymbolEqualityComparer.Default.Equals(p.Type, context))
            .Select(p => p.GetMethod)
            .Where(m => m is { DeclaredAccessibility: Accessibility.Public }).ToArray();
        var resolvers = context.GetMembers("GetTypeInfoFromHandle").OfType<IMethodSymbol>()
            .Where(m => !m.IsStatic && m.Arity == 0
                && m.DeclaredAccessibility == Accessibility.Public
                && m.Parameters.Length == 1 && m.Parameters[0].RefKind == RefKind.None
                && m.Parameters[0].Type.SpecialType == SpecialType.System_RuntimeTypeHandle
                && SymbolEqualityComparer.Default.Equals(m.ReturnType, info)).ToArray();
        return getters.Length == 1 && resolvers.Length == 1
            ? new RuntimeTypeOfBinding(info, getters[0]!, resolvers[0]) : null;
    }
}
