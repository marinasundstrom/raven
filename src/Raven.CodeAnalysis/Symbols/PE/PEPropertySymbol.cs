using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEPropertySymbol : PESymbol, IPropertySymbol
{
    private readonly PropertyInfo _propertyInfo;
    private ITypeSymbol _type;
    private Accessibility? _accessibility;

    public PEPropertySymbol(PropertyInfo propertyInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _propertyInfo = propertyInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;
    public override string Name => _propertyInfo.Name;

    public ITypeSymbol Type
    {
        get
        {
            if (_propertyInfo.PropertyType.IsGenericParameter)
            {
                return _type ??= new PETypeParameterSymbol(_propertyInfo.PropertyType, this, ContainingType, ContainingNamespace, []);
            }

            _type ??= PEContainingModule.GetType(_propertyInfo.PropertyType);

            var unionAttribute = _propertyInfo.GetCustomAttributesData().FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");
            if (unionAttribute is not null)
            {
                var types = ((IEnumerable<CustomAttributeTypedArgument>)unionAttribute.ConstructorArguments.First().Value).Select(x => (Type)x.Value);
                _type = new UnionTypeSymbol(types.Select(x => PEContainingModule.GetType(x)!).ToArray(), null, null, null, []);
            }

            return _type;
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