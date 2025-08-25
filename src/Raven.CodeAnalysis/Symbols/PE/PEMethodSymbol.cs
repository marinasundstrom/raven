using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEMethodSymbol : PESymbol, IMethodSymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly MethodBase _methodInfo;
    private ITypeSymbol? _returnType;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private Accessibility? _accessibility;

    public PEMethodSymbol(TypeResolver typeResolver, MethodBase methodInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _methodInfo = methodInfo;

        if (Name.StartsWith("op_Implicit") || Name.StartsWith("op_Explicit"))
        {
            MethodKind = MethodKind.Conversion;
        }
        else if (Name.StartsWith("op_"))
        {
            MethodKind = MethodKind.UserDefinedOperator;
        }
        else if (Name.StartsWith(".ctor"))
        {
            MethodKind = MethodKind.Constructor;
        }
        else if (Name.StartsWith(".cctor"))
        {
            MethodKind = MethodKind.StaticConstructor;
        }
        else if (Name.StartsWith("get_"))
        {
            MethodKind = MethodKind.PropertyGet;
        }
        else if (Name.StartsWith("set_"))
        {
            MethodKind = MethodKind.PropertySet;
        }
        else if (Name.StartsWith("add_"))
        {
            MethodKind = MethodKind.EventAdd;
        }
        else if (Name.StartsWith("remove_"))
        {
            MethodKind = MethodKind.EventRemove;
        }
        else
        {
            MethodKind = MethodKind.Ordinary;
        }
    }

    public PEMethodSymbol(TypeResolver typeResolver, MethodBase methodInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, Location[] locations)
    : base(containingSymbol, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _methodInfo = methodInfo;


        if (Name.StartsWith("op_Implicit") || Name.StartsWith("op_Explicit"))
        {
            MethodKind = MethodKind.Conversion;
        }
        else if (Name.StartsWith("op_"))
        {
            MethodKind = MethodKind.UserDefinedOperator;
        }
        else if (Name.StartsWith(".ctor"))
        {
            MethodKind = MethodKind.Constructor;
        }
        else if (Name.StartsWith(".cctor"))
        {
            MethodKind = MethodKind.StaticConstructor;
        }
        else if (Name.StartsWith("get_"))
        {
            MethodKind = MethodKind.PropertyGet;
        }
        else if (Name.StartsWith("set_"))
        {
            MethodKind = MethodKind.PropertySet;
        }
        else if (Name.StartsWith("add_"))
        {
            MethodKind = MethodKind.EventAdd;
        }
        else if (Name.StartsWith("remove_"))
        {
            MethodKind = MethodKind.EventRemove;
        }
        else
        {
            MethodKind = MethodKind.Ordinary;
        }
    }

    public override SymbolKind Kind => SymbolKind.Method;
    public override string Name => _methodInfo.Name;
    public ITypeSymbol ReturnType
    {
        get
        {
            if (_returnType == null)
            {
                var returnParam = ((MethodInfo)_methodInfo).ReturnParameter;
                _returnType = _typeResolver.ResolveType(returnParam)!;
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
                      _typeResolver,
                      param, this, this.ContainingType, this.ContainingNamespace,
                      [new MetadataLocation(ContainingModule!)]);
            }).OfType<IParameterSymbol>().ToImmutableArray();
        }
    }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_methodInfo);

    public override bool IsStatic => _methodInfo.IsStatic;

    public bool IsConstructor => _methodInfo.IsConstructor;

    public MethodKind MethodKind { get; }

    public IMethodSymbol? OriginalDefinition { get; }

    public bool IsAbstract => _methodInfo.IsAbstract;

    public bool IsAsync =>
        (_methodInfo as MethodInfo)?.ReturnType == typeof(Task) ||
        (_methodInfo as MethodInfo)?.ReturnType.IsGenericType == true &&
        (_methodInfo as MethodInfo)?.ReturnType.GetGenericTypeDefinition() == typeof(Task<>);

    public bool IsCheckedBuiltin => false; // No metadata indicator; default to false or customize

    public bool IsDefinition => true; // Metadata methods are always definitions

    public bool IsExtensionMethod =>
        _methodInfo?.GetCustomAttribute(typeof(ExtensionAttribute), false) is not null;

    public bool IsExtern => _methodInfo.IsAbstract || (_methodInfo.Attributes & MethodAttributes.PinvokeImpl) != 0;

    public bool IsGenericMethod => _methodInfo.IsGenericMethod;

    public bool IsOverride =>
        (_methodInfo as MethodInfo)?.GetBaseDefinition()?.DeclaringType != _methodInfo.DeclaringType;

    public bool IsReadOnly =>
        (_methodInfo as MethodInfo)?.ReturnParameter?.GetRequiredCustomModifiers()
            .Contains(typeof(IsReadOnlyAttribute)) == true;

    public bool IsSealed =>
        (_methodInfo as MethodInfo)?.IsFinal == true &&
        (_methodInfo as MethodInfo)?.IsVirtual == true;

    public bool IsVirtual => _methodInfo.IsVirtual;

    public MethodInfo GetMethodInfo() => (MethodInfo)_methodInfo;

    public ConstructorInfo GetConstructorInfo() => (ConstructorInfo)_methodInfo;
}