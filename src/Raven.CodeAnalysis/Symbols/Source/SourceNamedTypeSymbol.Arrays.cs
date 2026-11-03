using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceNamedTypeSymbol
{
    bool IArrayTypeProvider.AreInterfacesComplete => GetDeclaringCompilation()?.SourceDeclarationsComplete != false;

    private NamedTypeSymbol? ArrayShape(ArrayTypeSymbol array)
    {
        var compilation = GetDeclaringCompilation();
        if (compilation?.Options.TargetPlatform != TargetPlatform.NeoCLR || compilation.Options.MetadataImportOptions?.UseSourceCore != true ||
            SpecialType != SpecialType.System_Array || array.Rank != 1 ||
            compilation.Options.RuntimeIterationContract is not { ArrayShapeTypeName: { } name } contract)
            return null;
        var shape = compilation.GetTypeByMetadataName(name);
        return shape is { TypeKind: TypeKind.Class, Arity: 1 } && shape.ContainingAssembly?.Name == contract.AssemblyName
            ? shape.Construct(array.ElementType) as NamedTypeSymbol : null;
    }

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
