using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class ArrayTypeSymbol : MetadataSymbol, IArrayTypeSymbol
{
    public ArrayTypeSymbol(Compilation compilation, ITypeSymbol elementType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(compilation, containingSymbol, containingType, containingNamespace, locations)
    {
        ElementType = elementType;
    }

    public ITypeSymbol ElementType { get; }

    public SpecialType SpecialType => SpecialType.System_Array;

    public bool IsValueType => false;

    public bool IsNamespace => false;

    public bool IsType => true;

    public ImmutableArray<ISymbol> GetMembers()
    {
        // TODO: Fix
        return [];
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        // TODO: Fix
        return [];
    }
}