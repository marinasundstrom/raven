using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEArrayTypeSymbol : PENamedTypeSymbol, IArrayTypeSymbol
{
    private ITypeSymbol? _elementType;

    public PEArrayTypeSymbol(ReflectionTypeLoader reflectionTypeLoader, System.Reflection.TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, int rank = 1)
        : base(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
    {

    }

    public override SymbolKind Kind => SymbolKind.Type;

    public ITypeSymbol ElementType
    {
        get
        {
            if (_elementType is not null)
                return _elementType;

            var runtimeElementType = _typeInfo.GetElementType()
                ?? throw new InvalidOperationException($"Array type '{_typeInfo}' has no element type.");

            return _elementType = PEContainingModule.GetType(runtimeElementType)
                ?? throw new InvalidOperationException($"Could not resolve element type '{runtimeElementType}' for array type '{_typeInfo}'.");
        }
    }

    public int Rank => _typeInfo.GetArrayRank();

    public bool IsFixedArray => false;

    public int? FixedLength => null;

    public override string ToString()
    {
        return Name;
    }
}
