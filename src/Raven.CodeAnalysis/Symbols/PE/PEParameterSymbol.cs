using System;
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

    public bool IsParams
    {
        get
        {
            return
            _parameterInfo.GetCustomAttributesData()
                 .Any(attr => attr.AttributeType.FullName == "System.ParamArrayAttribute");
        }
    }

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

    public bool IsMutable => RefKind is RefKind.Ref or RefKind.Out;

    public ParameterInfo GetParameterInfo() => _parameterInfo;

    public bool HasExplicitDefaultValue
    {
        get
        {
            var rawDefaultValue = _parameterInfo.RawDefaultValue;
            return rawDefaultValue != DBNull.Value && rawDefaultValue != System.Type.Missing;
        }
    }

    public object? ExplicitDefaultValue
    {
        get
        {
            var rawDefaultValue = _parameterInfo.RawDefaultValue;
            if (rawDefaultValue == DBNull.Value || rawDefaultValue == System.Type.Missing)
                return null;

            return rawDefaultValue;
        }
    }
}
