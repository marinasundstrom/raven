using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataFieldSymbol : MetadataSymbol, IFieldSymbol
{
    private readonly PropertyInfo _fieldInfo;

    public MetadataFieldSymbol(PropertyInfo fieldInfo,  ITypeSymbol fieldType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _fieldInfo = fieldInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;
    public override string Name => _fieldInfo.Name;
    
    public ITypeSymbol FieldType { get; }
}