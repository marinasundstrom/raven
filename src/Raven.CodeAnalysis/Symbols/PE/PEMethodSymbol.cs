using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEMethodSymbol : PESymbol, IMethodSymbol
{
    private readonly MethodBase _methodInfo;
    private ITypeSymbol? _returnType;
    private ImmutableArray<IParameterSymbol>? _parameters;

    public PEMethodSymbol(MethodBase methodInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _methodInfo = methodInfo;
    }

    public PEMethodSymbol(MethodBase methodInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, Location[] locations)
    : base(containingSymbol, containingType, containingType.ContainingNamespace, locations)
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
                    _returnType = PEContainingAssembly.GetTypeByMetadataName("System.Void");
                }
                else
                {
                    _returnType = PEContainingModule.GetType(((MethodInfo)_methodInfo).ReturnType);
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
                var t = PEContainingModule.GetType(param.ParameterType);

                return new PEParameterSymbol(
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