using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PENamedTypeSymbol
{
    ImmutableArray<ISymbol> IArrayTypeProvider.GetMembers(IArrayTypeSymbol array)
    {
        var members = GetMembers();
        if (array.Rank != 1 || Compilation.Options.RuntimeIterationContract?.ArrayShapeTypeName is null)
            return members;
        // Interface members keep their interface owner so calls use normal dispatch.
        var projected = ((IArrayTypeProvider)this).GetAdditionalInterfaces(array).SelectMany(i => i.GetMembers())
            .Where(m => !m.IsStatic).ToImmutableArray();
        var contract = Compilation.Options.RuntimeIterationContract!;
        var shape = Compilation.GetTypeByMetadataName(contract.ArrayShapeTypeName!);
        if (shape is { TypeKind: TypeKind.Class, Arity: 1 } &&
            shape.ContainingAssembly?.Name == contract.AssemblyName &&
            shape.Construct(array.ElementType) is INamedTypeSymbol constructedShape)
        {
            // Retain the metadata owner for real target members. A vector's storage and
            // signatures stay arrays; member references name the configured generic shape.
            projected = projected.AddRange(constructedShape.GetMembers()
                .Where(m => m is IMethodSymbol or IPropertySymbol &&
                    m.DeclaredAccessibility == Accessibility.Public &&
                    m is not IMethodSymbol { MethodKind: MethodKind.Constructor or MethodKind.StaticConstructor } &&
                    !projected.Any(existing => existing.Name == m.Name)));
        }
        return projected.AddRange(members.Where(m => !projected.Any(p => p.Name == m.Name)));
    }

    ImmutableArray<INamedTypeSymbol> IArrayTypeProvider.GetAdditionalInterfaces(IArrayTypeSymbol array)
    {
        if (array.Rank != 1)
        {
            return ImmutableArray<INamedTypeSymbol>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();

        // A target can describe its vector interfaces on a regular generic class.
        // Explicit but invalid metadata must not invent host-runtime interfaces.
        if (Compilation.Options.RuntimeIterationContract is { ArrayShapeTypeName: not null } shapeContract)
        {
            var shape = Compilation.GetTypeByMetadataName(shapeContract.ArrayShapeTypeName);
            if (shape is { TypeKind: TypeKind.Class, Arity: 1 } &&
                shape.ContainingAssembly?.Name == shapeContract.AssemblyName &&
                shape.Construct(array.ElementType) is INamedTypeSymbol constructedShape)
            {
                foreach (var implemented in constructedShape.AllInterfaces)
                    AddUniqueArrayInterface(builder, implemented);
            }
            return builder.ToImmutable();
        }

        if (Compilation.Options.RuntimeIterationContract is { ArraysImplementIterable: true } contract)
        {
            var definition = Compilation.GetTypeByMetadataName(contract.IterableTypeName);
            if (definition is { TypeKind: TypeKind.Interface, Arity: 1 } &&
                definition.ContainingAssembly?.Name == contract.AssemblyName &&
                definition.Construct(array.ElementType) is INamedTypeSymbol constructed)
                AddUniqueArrayInterface(builder, constructed);
            return builder.ToImmutable();
        }

        AddConstructedArrayInterface(array, builder, "System.Collections.Generic.IEnumerable`1");
        AddConstructedArrayInterface(array, builder, "System.Collections.Generic.ICollection`1");
        AddConstructedArrayInterface(array, builder, "System.Collections.Generic.IList`1");
        AddConstructedArrayInterface(array, builder, "System.Collections.Generic.IReadOnlyCollection`1");
        AddConstructedArrayInterface(array, builder, "System.Collections.Generic.IReadOnlyList`1");

        return builder.ToImmutable();
    }

    private void AddConstructedArrayInterface(IArrayTypeSymbol array, ImmutableArray<INamedTypeSymbol>.Builder builder, string metadataName)
    {
        if (TryResolveArrayInterface(array, metadataName) is not INamedTypeSymbol definition)
            return;

        if (!definition.IsGenericType || definition.Arity != 1)
            return;

        if (definition.Construct(array.ElementType) is not INamedTypeSymbol constructed)
            return;

        AddUniqueArrayInterface(builder, constructed);
    }

    private static INamedTypeSymbol? TryResolveArrayInterface(IArrayTypeSymbol array, string metadataName)
    {
        if (array.BaseType?.ContainingAssembly?.GetTypeByMetadataName(metadataName) is INamedTypeSymbol resolved)
            return resolved;

        return array.ContainingAssembly?.GetTypeByMetadataName(metadataName);
    }

    private static void AddUniqueArrayInterface(ImmutableArray<INamedTypeSymbol>.Builder builder, INamedTypeSymbol symbol)
    {
        foreach (var existing in builder)
        {
            if (SymbolEqualityComparer.Default.Equals(existing, symbol))
                return;
        }

        builder.Add(symbol);
    }
}
