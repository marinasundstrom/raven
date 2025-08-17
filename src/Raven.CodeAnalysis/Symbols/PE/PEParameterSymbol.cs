using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEParameterSymbol : PESymbol, IParameterSymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly ParameterInfo _parameterInfo;
    private ITypeSymbol _type;

    public PEParameterSymbol(TypeResolver typeResolver, ParameterInfo parameterInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _parameterInfo = parameterInfo;
    }

    public override SymbolKind Kind => SymbolKind.Parameter;
    public override string Name => _parameterInfo.Name;
    public ITypeSymbol Type
    {
        get
        {
            return _type ??= _typeResolver.ResolveType(_parameterInfo);
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