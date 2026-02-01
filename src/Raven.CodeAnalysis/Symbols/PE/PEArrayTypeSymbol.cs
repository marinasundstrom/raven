using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEArrayTypeSymbol : PENamedTypeSymbol, IArrayTypeSymbol
{
    private INamedTypeSymbol? _elementType;

    public PEArrayTypeSymbol(ReflectionTypeLoader reflectionTypeLoader, System.Reflection.TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, int rank = 1)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
    {

    }

    public override SymbolKind Kind => SymbolKind.Type;

    public ITypeSymbol ElementType => _elementType ??= (_typeInfo.HasElementType ? (INamedTypeSymbol?)PEContainingModule.GetType(_typeInfo.GetElementType()!) : null);

    public int Rank => _typeInfo.GetArrayRank();

    public override string ToString()
    {
        return Name;
    }
}
