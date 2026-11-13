using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceNamedTypeSymbol
{
    bool IArrayTypeProvider.AreInterfacesComplete => GetDeclaringServices()?.SourceDeclarationsComplete != false;

    private NamedTypeSymbol? ArrayShape(ArrayTypeSymbol array)
        => (GetDeclaringServices() as SourceTypeDeclarationServices)?.GetArrayShape(this, array);

    ImmutableArray<NamedTypeSymbol> IArrayTypeProvider.GetAdditionalInterfaces(ArrayTypeSymbol array)
        => ArrayShape(array)?.AllInterfaces ?? [];

    ImmutableArray<Symbol> IArrayTypeProvider.GetMembers(ArrayTypeSymbol array)
    {
        if (ArrayShape(array) is not { } shape) return GetMembers();
        var projected = shape.AllInterfaces.SelectMany(i => i.GetMembers()).Where(m => !m.IsStatic).ToImmutableArray();
        projected = projected.AddRange(shape.GetMembers().Where(m => m is MethodSymbol or PropertySymbol &&
            m.DeclaredAccessibility == Accessibility.Public &&
            m is not MethodSymbol { MethodKind: MethodKind.Constructor or MethodKind.StaticConstructor } &&
            !projected.Any(existing => existing.Name == m.Name)));
        return projected.AddRange(GetMembers().Where(m => !projected.Any(p => p.Name == m.Name)));
    }
}
