using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class ArrayTypeSymbol : PESymbol, IArrayTypeSymbol
{
    public ArrayTypeSymbol(INamedTypeSymbol baseType, ITypeSymbol elementType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, int rank = 1)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        BaseType = baseType;
        ElementType = elementType;
        Rank = rank;

        TypeKind = TypeKind.Array;
    }

    public override string Name => $"{ElementType}[]";

    public override SymbolKind Kind => SymbolKind.Type;

    public ITypeSymbol ElementType { get; }

    public SpecialType SpecialType => SpecialType.System_Array;

    public bool IsNamespace => false;

    public bool IsType => true;

    public int Rank { get; }

    public INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public ImmutableArray<ISymbol> GetMembers()
    {
        return BaseType!.GetMembers();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return BaseType!.GetMembers(name);
    }

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        return Name;
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotSupportedException();
    }
}