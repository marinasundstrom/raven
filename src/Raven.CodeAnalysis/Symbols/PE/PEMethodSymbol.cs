using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEMethodSymbol : PESymbol, IMethodSymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly MethodBase _methodInfo;
    private ITypeSymbol? _returnType;
    private ImmutableArray<IParameterSymbol>? _parameters;
    private ImmutableArray<ITypeParameterSymbol>? _typeParameters;
    private ImmutableArray<ITypeSymbol>? _typeArguments;
    private Accessibility? _accessibility;

    public PEMethodSymbol(TypeResolver typeResolver, MethodBase methodInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _methodInfo = methodInfo;
        _typeResolver.RegisterMethodSymbol(_methodInfo, this);

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
        _typeResolver.RegisterMethodSymbol(_methodInfo, this);


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

    public override string MetadataName => _methodInfo.Name;
    public ITypeSymbol ReturnType
    {
        get
        {
            if (_returnType == null)
            {
                if (_methodInfo is MethodInfo methodInfo)
                {
                    var returnParam = methodInfo.ReturnParameter;
                    _returnType = _typeResolver.ResolveType(returnParam.ParameterType, _methodInfo)!;
                }
                else if (MethodKind is MethodKind.Constructor or MethodKind.StaticConstructor)
                {
                    var declaringType = _methodInfo.DeclaringType ?? throw new InvalidOperationException($"Constructor '{_methodInfo}' is missing a declaring type.");
                    _returnType = _typeResolver.ResolveType(declaringType, _methodInfo)!;
                }
                else
                {
                    throw new InvalidOperationException($"Unsupported method base type '{_methodInfo.GetType()}'.");
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
                      _typeResolver,
                      param, this, this.ContainingType, this.ContainingNamespace,
                      [new MetadataLocation(ContainingModule!)]);
            }).OfType<IParameterSymbol>().ToImmutableArray();
        }
    }

    public ImmutableArray<AttributeData> GetReturnTypeAttributes() => ImmutableArray<AttributeData>.Empty;

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_methodInfo);

    public override bool IsStatic => _methodInfo.IsStatic;

    public bool IsConstructor => _methodInfo.IsConstructor;

    public MethodKind MethodKind { get; }

    public IMethodSymbol? OriginalDefinition => this;

    public bool IsAbstract => _methodInfo.IsAbstract;

    public bool IsAsync =>
        (_methodInfo as MethodInfo)?.ReturnType == typeof(Task) ||
        (_methodInfo as MethodInfo)?.ReturnType.IsGenericType == true &&
        (_methodInfo as MethodInfo)?.ReturnType.GetGenericTypeDefinition() == typeof(Task<>);

    public bool IsCheckedBuiltin => false; // No metadata indicator; default to false or customize

    public bool IsDefinition => true; // Metadata methods are always definitions

    private bool? _lazyIsExtensionMethod;

    public bool IsExtensionMethod => _lazyIsExtensionMethod ??= ComputeIsExtensionMethod();

    private bool ComputeIsExtensionMethod()
    {
        if (_methodInfo is null || !_methodInfo.IsStatic)
            return false;

        try
        {
            if (HasExtensionAttribute(_methodInfo.GetCustomAttributesData()))
                return true;

            var declaringType = _methodInfo.DeclaringType;
            if (declaringType is null)
                return false;

            return HasExtensionAttribute(declaringType.GetCustomAttributesData());
        }
        catch (Exception)
        {
            return false;
        }
    }

    private static bool HasExtensionAttribute(IList<CustomAttributeData> attributes)
    {
        foreach (var attribute in attributes)
        {
            if (attribute.AttributeType.FullName == typeof(ExtensionAttribute).FullName)
                return true;
        }

        return false;
    }

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

    public bool IsIterator => false;

    public IteratorMethodKind IteratorKind => IteratorMethodKind.None;

    public ITypeSymbol? IteratorElementType => null;

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => ImmutableArray<IMethodSymbol>.Empty;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters =>
        _typeParameters ??= !_methodInfo.IsGenericMethodDefinition
            ? ImmutableArray<ITypeParameterSymbol>.Empty
            : ((MethodInfo)_methodInfo).GetGenericArguments()
                .Select(t => (ITypeParameterSymbol)_typeResolver.ResolveMethodTypeParameter(t, this)!)
                .ToImmutableArray();

    public ImmutableArray<ITypeSymbol> TypeArguments =>
        _typeArguments ??= TypeParameters.IsDefaultOrEmpty
            ? ImmutableArray<ITypeSymbol>.Empty
            : TypeParameters.Select(static tp => (ITypeSymbol)tp).ToImmutableArray();

    public IMethodSymbol? ConstructedFrom => this;

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments is null)
            throw new ArgumentNullException(nameof(typeArguments));

        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray());
    }
}
