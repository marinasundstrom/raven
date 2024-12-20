using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataTypeSymbol : MetadataSymbol, ITypeSymbol, INamedTypeSymbol
{
    private readonly TypeInfo _typeInfo;

    public MetadataTypeSymbol(TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _typeInfo = typeInfo;
    }

    public override SymbolKind Kind => SymbolKind.Type;
    public override string Name => _typeInfo.Name;
}