using System;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEParameterSymbol : PESymbol, IParameterSymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly ParameterInfo _parameterInfo;
    private ITypeSymbol _type;
    private bool _defaultValueComputed;
    private bool _hasExplicitDefaultValue;
    private object? _explicitDefaultValue;

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
            EnsureDefaultValueComputed();
            return _hasExplicitDefaultValue;
        }
    }

    public object? ExplicitDefaultValue
    {
        get
        {
            EnsureDefaultValueComputed();
            return _explicitDefaultValue;
        }
    }

    private void EnsureDefaultValueComputed()
    {
        if (_defaultValueComputed)
            return;

        var rawDefaultValue = _parameterInfo.RawDefaultValue;
        if (rawDefaultValue != DBNull.Value && rawDefaultValue != System.Type.Missing)
        {
            _hasExplicitDefaultValue = true;
            _explicitDefaultValue = rawDefaultValue;
        }
        else
        {
            bool hasOptionalAttribute = false;

            foreach (var attributeData in _parameterInfo.GetCustomAttributesData())
            {
                var attributeTypeName = attributeData.AttributeType.FullName;

                if (attributeTypeName == "System.Runtime.InteropServices.DefaultParameterValueAttribute")
                {
                    if (attributeData.ConstructorArguments.Count > 0)
                    {
                        _hasExplicitDefaultValue = true;
                        _explicitDefaultValue = attributeData.ConstructorArguments[0].Value;
                    }

                    break;
                }

                if (attributeTypeName == "System.Runtime.InteropServices.OptionalAttribute")
                {
                    hasOptionalAttribute = true;
                }
            }

            if (!_hasExplicitDefaultValue && hasOptionalAttribute)
            {
                _hasExplicitDefaultValue = true;
                _explicitDefaultValue = null;
            }
        }

        _defaultValueComputed = true;
    }
}
