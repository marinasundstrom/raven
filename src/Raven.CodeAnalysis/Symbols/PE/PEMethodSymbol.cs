using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEMethodSymbol : PESymbol, IMethodSymbol
{
    private readonly MethodBase _methodInfo;
    private ITypeSymbol? _returnType;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private Accessibility? _accessibility;

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
                    var returnParam = ((MethodInfo)_methodInfo).ReturnParameter;

                    _returnType = PEContainingModule.GetType(returnParam.ParameterType);

                    var unionAttribute = returnParam.GetCustomAttributesData().FirstOrDefault(x => x.AttributeType.Name == "TypeUnionAttribute");
                    if (unionAttribute is not null)
                    {
                        var types = ((IEnumerable<CustomAttributeTypedArgument>)unionAttribute.ConstructorArguments.First().Value).Select(x => (Type)x.Value);
                        _returnType = new UnionTypeSymbol(types.Select(x => PEContainingModule.GetType(x)!).ToArray(), null, null, null, []);
                    }
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
                return new PEParameterSymbol(
                      param, this, this.ContainingType, this.ContainingNamespace,
                      [new MetadataLocation(ContainingModule!)]);
            }).OfType<IParameterSymbol>().ToImmutableArray();
        }
    }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_methodInfo);

    public override bool IsStatic => _methodInfo.IsStatic;

    public bool IsConstructor => _methodInfo.IsConstructor;

    public MethodInfo GetMethodInfo() => (MethodInfo)_methodInfo;

    public ConstructorInfo GetConstructorInfo() => (ConstructorInfo)_methodInfo;
}