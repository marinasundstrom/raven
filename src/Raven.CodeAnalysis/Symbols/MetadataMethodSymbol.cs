using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal class MetadataMethodSymbol : MetadataSymbol, IMethodSymbol
{
    private readonly MethodBase _methodInfo;
    private ITypeSymbol? _returnType;
    private ImmutableArray<IParameterSymbol>? _parameters;

    public MetadataMethodSymbol(Compilation compilation, MethodBase methodInfo, ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(compilation, containingSymbol, containingType, containingNamespace, locations)
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

                _returnType = _compilation.GetType(((MethodInfo)_methodInfo).ReturnType);

                /*
                _returnType = _methodInfo.ReturnType == typeof(void)
                    ? _compilation.GetSpecialType(SpecialType.System_Void)
                    : _compilation.GetType(_methodInfo.ReturnType);
                    */
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
                var t = _compilation.GetType(param.ParameterType);

                return new MetadataParameterSymbol(_compilation,
                      param, null, this, this.ContainingType, this.ContainingNamespace,
                      []);
            }).OfType<IParameterSymbol>().ToImmutableArray();
        }
    }

    public override bool IsStatic => _methodInfo.IsStatic;

    public MethodInfo GetMethodInfo() => (MethodInfo)_methodInfo;

    public ConstructorInfo GetConstructorInfo() => (ConstructorInfo)_methodInfo;
}