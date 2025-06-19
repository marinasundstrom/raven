using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEPropertySymbol : PESymbol, IPropertySymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly PropertyInfo _propertyInfo;
    private ITypeSymbol _type;
    private Accessibility? _accessibility;

    public PEPropertySymbol(TypeResolver typeResolver, PropertyInfo propertyInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _propertyInfo = propertyInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;
    public override string Name => _propertyInfo.Name;

    public ITypeSymbol Type
    {
        get
        {
            return _type ??= _typeResolver.ResolveType(_propertyInfo);
        }
    }

    public IMethodSymbol? GetMethod { get; set; }
    public IMethodSymbol? SetMethod { get; set; }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_propertyInfo);

    public override bool IsStatic => (_propertyInfo.GetMethod?.IsStatic ?? false) || (_propertyInfo.SetMethod?.IsStatic ?? false);

    public bool IsIndexer => _propertyInfo.GetIndexParameters().Length > 0;

    public PropertyInfo GetPropertyInfo()
    {
        return _propertyInfo;
    }
}