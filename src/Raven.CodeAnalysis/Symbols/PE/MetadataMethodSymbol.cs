using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class MetadataMethodSymbol : MetadataSymbol, IMethodSymbol
{
    private readonly MethodBase _methodInfo;
    private ITypeSymbol? _returnType;
    private ImmutableArray<IParameterSymbol>? _parameters;

    public MetadataMethodSymbol(MethodBase methodInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(containingSymbol, containingType, containingNamespace, locations)
    {
        _methodInfo = methodInfo;
    }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => _methodInfo.Name;
    public ITypeSymbol ReturnType
    {
        get
        {
            if (_returnType == null)
            {
                if (_methodInfo is ConstructorInfo)
                {
                    _returnType = Compilation.GetSpecialType(SpecialType.System_Void);
                }
                else
                {
                    _returnType = Compilation.GetType(((MethodInfo)_methodInfo).ReturnType);
                }
            }
            return _returnType;
        }
    }

    public ImmutableArray<IParameterSymbol> Parameters
    {
        get
        {
            return _parameters ??= _methodInfo.GetParameters().Select(param =>
            {
                var t = Compilation.GetType(param.ParameterType);

                return new MetadataParameterSymbol(
                      param, null, this, this.ContainingType, this.ContainingNamespace,
                      [new MetadataLocation()]);
            }).OfType<IParameterSymbol>().ToImmutableArray();
        }
    }

    public override bool IsStatic => _methodInfo.IsStatic;

    public bool IsConstructor => _methodInfo.IsConstructor;

    public MethodInfo GetMethodInfo() => (MethodInfo)_methodInfo;

    public ConstructorInfo GetConstructorInfo() => (ConstructorInfo)_methodInfo;
}