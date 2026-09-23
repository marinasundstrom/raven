using System.Linq;
using System.Collections.Generic;

namespace Raven.CodeAnalysis;

// Opt-in target protocol; never inferred from Task names or payload types.
internal static class AsyncCancellationProtocol
{
    public static IPropertySymbol? FindIsCancelled(ITypeSymbol type) => Unique(type.GetMembers("IsCancelled")
        .OfType<IPropertySymbol>().Where(p => !p.IsStatic && p.Parameters.Length == 0
            && p.Type.SpecialType == SpecialType.System_Boolean
            && p.GetMethod is { DeclaredAccessibility: Accessibility.Public, IsStatic: false }));

    public static IMethodSymbol? FindSetCancelled(ITypeSymbol type) => Unique(type.GetMembers("SetCancelled")
        .OfType<IMethodSymbol>().Where(m => !m.IsStatic && !m.IsGenericMethod
            && m.Parameters.Length == 0 && m.ReturnType.SpecialType is SpecialType.System_Void or SpecialType.System_Unit
            && m.DeclaredAccessibility == Accessibility.Public));
    private static T? Unique<T>(IEnumerable<T> members) where T : class
    {
        var candidates = members.Take(2).ToArray();
        return candidates.Length == 1 ? candidates[0] : null;
    }
}
