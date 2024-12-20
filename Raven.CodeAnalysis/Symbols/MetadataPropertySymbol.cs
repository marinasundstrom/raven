using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataPropertySymbol : MetadataSymbol, IPropertySymbol
{
    private readonly PropertyInfo _propertyInfo;

    public MetadataPropertySymbol(PropertyInfo propertyInfo,  ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _propertyInfo = propertyInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;
    public override string Name => _propertyInfo.Name;
    
    public ITypeSymbol PropertyType { get; }
}