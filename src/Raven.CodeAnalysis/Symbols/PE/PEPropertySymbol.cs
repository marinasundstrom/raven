using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEPropertySymbol : PESymbol, IPropertySymbol
{
    private readonly PropertyInfo _propertyInfo;
    private ITypeSymbol _type;

    public PEPropertySymbol(PropertyInfo propertyInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _propertyInfo = propertyInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;
    public override string Name => _propertyInfo.Name;

    public ITypeSymbol Type => _type ??= PEContainingModule.GetType(_propertyInfo.PropertyType);
    public IMethodSymbol? GetMethod { get; set; }
    public IMethodSymbol? SetMethod { get; set; }

    public override bool IsStatic => (_propertyInfo.GetMethod?.IsStatic ?? false) || (_propertyInfo.SetMethod?.IsStatic ?? false);

    public bool IsIndexer => _propertyInfo.GetIndexParameters().Length > 0;
}