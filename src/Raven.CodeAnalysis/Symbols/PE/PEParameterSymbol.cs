using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEParameterSymbol : PESymbol, IParameterSymbol
{
    private readonly ParameterInfo _parameterInfo;
    private ITypeSymbol _type;

    public PEParameterSymbol(ParameterInfo parameterInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _parameterInfo = parameterInfo;
    }

    public override SymbolKind Kind => SymbolKind.Parameter;
    public override string Name => _parameterInfo.Name;
    public ITypeSymbol Type
    {
        get
        {
            if (_type is not null) return _type;

            if (_parameterInfo.ParameterType.IsGenericTypeParameter || _parameterInfo.ParameterType.IsGenericMethodParameter)
            {
                _type = new PETypeParameterSymbol(_parameterInfo.ParameterType, this, ContainingType, ContainingNamespace, []);
                return _type;
            }

            _type = PEContainingModule.GetType(_parameterInfo.ParameterType);

            var unionAttribute = _parameterInfo.GetCustomAttributesData().FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");
            if (unionAttribute is not null)
            {
                var types = ((IEnumerable<CustomAttributeTypedArgument>)unionAttribute.ConstructorArguments.First().Value).Select(x => (Type)x.Value);
                _type = new UnionTypeSymbol(types.Select(x => PEContainingModule.GetType(x)!).ToArray(), null, null, null, []);
            }

            return _type;
        }
    }


    public bool IsParams => _parameterInfo.GetCustomAttributes(typeof(ParamArrayAttribute), false).Length > 0;

    public RefKind RefKind
    {
        get
        {
            if (_parameterInfo.IsIn) return RefKind.In;
            if (_parameterInfo.IsOut) return RefKind.Out;
            if (_parameterInfo.ParameterType.IsByRef) return RefKind.Ref;
            return RefKind.None;
        }
    }

    public ParameterInfo GetParameterInfo() => _parameterInfo;
}