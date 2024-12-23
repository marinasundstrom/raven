using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataFieldSymbol : MetadataSymbol, IFieldSymbol
{
    private readonly PropertyInfo _fieldInfo;

    public MetadataFieldSymbol(Compilation compilation, PropertyInfo fieldInfo, ITypeSymbol fieldType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(compilation, containingSymbol, containingType, containingNamespace, locations)
    {
        _fieldInfo = fieldInfo;
    }

    public override SymbolKind Kind => SymbolKind.Field;
    public override string Name => _fieldInfo.Name;

    public ITypeSymbol Type { get; }
}