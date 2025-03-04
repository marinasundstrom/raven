using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class ArrayTypeSymbol : MetadataSymbol, IArrayTypeSymbol
{
    private INamedTypeSymbol? _baseType;

    public ArrayTypeSymbol(Compilation compilation, ITypeSymbol elementType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(compilation, containingSymbol, containingType, containingNamespace, locations)
    {
        ElementType = elementType;
    }

    public override string Name => $"{ElementType}[]";

    public override SymbolKind Kind => SymbolKind.Type;

    public ITypeSymbol ElementType { get; }

    public SpecialType SpecialType => SpecialType.System_Array;

    public bool IsValueType => false;

    public bool IsNamespace => false;

    public bool IsType => true;

    public INamedTypeSymbol? BaseType => _baseType ??= (INamedTypeSymbol?)_compilation.GetSpecialType(SpecialType.System_Array);

    public ImmutableArray<ISymbol> GetMembers()
    {
        return BaseType!.GetMembers();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        return BaseType!.GetMembers(name);
    }

    public override string ToString()
    {
        return Name;
    }
}