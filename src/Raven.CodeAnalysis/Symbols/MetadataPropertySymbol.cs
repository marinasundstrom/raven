using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class MetadataPropertySymbol : MetadataSymbol, IPropertySymbol
{
    private readonly PropertyInfo _propertyInfo;
    private ITypeSymbol _type;

    public MetadataPropertySymbol(PropertyInfo propertyInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _propertyInfo = propertyInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;
    public override string Name => _propertyInfo.Name;

    public ITypeSymbol Type => _type ??= Compilation.GetType(_propertyInfo.PropertyType);
    public IMethodSymbol? GetMethod { get; set; }
    public IMethodSymbol? SetMethod { get; set; }

    public override bool IsStatic => (_propertyInfo.GetMethod?.IsStatic ?? false) || (_propertyInfo.SetMethod?.IsStatic ?? false);

    public bool IsIndexer => _propertyInfo.GetIndexParameters().Length > 0;
}