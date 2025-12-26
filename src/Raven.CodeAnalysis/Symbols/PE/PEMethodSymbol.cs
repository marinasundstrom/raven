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
    private ImmutableArray<IMethodSymbol>? _explicitInterfaceImplementations;

    public PEMethodSymbol(TypeResolver typeResolver, MethodBase methodInfo, INamedTypeSymbol? containingType, Location[] locations, ISymbol? associatedSymbol = null)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _methodInfo = methodInfo;
        _typeResolver.RegisterMethodSymbol(_methodInfo, this);

        var name = _methodInfo.Name;

        if (name.StartsWith("op_Implicit") || name.StartsWith("op_Explicit"))
        {
            MethodKind = MethodKind.Conversion;
        }
        else if (name.StartsWith("op_"))
        {
            MethodKind = MethodKind.UserDefinedOperator;
        }
        else if (name.StartsWith(".ctor"))
        {
            MethodKind = MethodKind.Constructor;
        }
        else if (name.StartsWith(".cctor"))
        {
            MethodKind = MethodKind.StaticConstructor;
        }
        else if (name.StartsWith("get_"))
        {
            MethodKind = MethodKind.PropertyGet;
        }
        else if (name.StartsWith("set_"))
        {
            MethodKind = MethodKind.PropertySet;
        }
        else if (name.StartsWith("add_"))
        {
            MethodKind = MethodKind.EventAdd;
        }
        else if (name.StartsWith("remove_"))
        {
            MethodKind = MethodKind.EventRemove;
        }
        else if (name.Contains('.'))
        {
            MethodKind = MethodKind.ExplicitInterfaceImplementation;
        }
        else
        {
            MethodKind = MethodKind.Ordinary;
        }

        AssociatedSymbol = associatedSymbol;
    }

    public PEMethodSymbol(TypeResolver typeResolver, MethodBase methodInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, Location[] locations, ISymbol? associatedSymbol = null)
    : base(containingSymbol, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _methodInfo = methodInfo;
        _typeResolver.RegisterMethodSymbol(_methodInfo, this);

        var name = _methodInfo.Name;

        if (name.StartsWith("op_Implicit") || name.StartsWith("op_Explicit"))
        {
            MethodKind = MethodKind.Conversion;
        }
        else if (name.StartsWith("op_"))
        {
            MethodKind = MethodKind.UserDefinedOperator;
        }
        else if (name.StartsWith(".ctor"))
        {
            MethodKind = MethodKind.Constructor;
        }
        else if (name.StartsWith(".cctor"))
        {
            MethodKind = MethodKind.StaticConstructor;
        }
        else if (name.StartsWith("get_"))
        {
            MethodKind = MethodKind.PropertyGet;
        }
        else if (name.StartsWith("set_"))
        {
            MethodKind = MethodKind.PropertySet;
        }
        else if (name.StartsWith("add_"))
        {
            MethodKind = MethodKind.EventAdd;
        }
        else if (name.StartsWith("remove_"))
        {
            MethodKind = MethodKind.EventRemove;
        }
        else if (name.Contains('.'))
        {
            MethodKind = MethodKind.ExplicitInterfaceImplementation;
        }
        else
        {
            MethodKind = MethodKind.Ordinary;
        }

        AssociatedSymbol = associatedSymbol;
    }

    public override SymbolKind Kind => SymbolKind.Method;

    public override string Name
    {
        get
        {
            if (_name is not null)
            {
                return _name;
            }
            if (!_methodInfo.Name.Contains(".ctor")
                && !_methodInfo.Name.Contains(".cctor")
                && _methodInfo.Name.Contains('.'))
            {
                _name = _methodInfo.Name.Split('.').Last();
            }
            else
            {
                _name = _methodInfo.Name;
            }
            return _name;
        }
    }

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
                    _returnType = _typeResolver.ResolveType(typeof(void));
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

    public ISymbol? AssociatedSymbol { get; }

    public bool IsAbstract => _methodInfo.IsAbstract;

    public bool IsAsync =>
        (_methodInfo as MethodInfo)?.ReturnType == typeof(Task) ||
        (_methodInfo as MethodInfo)?.ReturnType.IsGenericType == true &&
        (_methodInfo as MethodInfo)?.ReturnType.GetGenericTypeDefinition() == typeof(Task<>);

    public bool IsCheckedBuiltin => false; // No metadata indicator; default to false or customize

    public bool IsDefinition => true; // Metadata methods are always definitions

    private bool? _lazyIsExtensionMethod;
    private string? _name;

    public bool IsExtensionMethod => _lazyIsExtensionMethod ??= ComputeIsExtensionMethod();

    private bool ComputeIsExtensionMethod()
    {
        if (_methodInfo is null || !_methodInfo.IsStatic)
            return false;

        if (Parameters.IsDefaultOrEmpty || Parameters.Length == 0)
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

    public bool IsOverride
    {
        get
        {
            try
            {
                return (_methodInfo as MethodInfo)?.GetBaseDefinition()?.DeclaringType != _methodInfo.DeclaringType;
            }
            catch { return false; }
        }
    }

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

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            // Fast-path: most methods are not explicit impls
            if (!_methodInfo.Name.Contains('.'))
            {
                _explicitInterfaceImplementations = ImmutableArray<IMethodSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var declaringType = _methodInfo.DeclaringType;
            if (declaringType is null)
            {
                _explicitInterfaceImplementations = ImmutableArray<IMethodSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            // Metadata name is something like "Namespace.IFoo`1.Bar"
            var metadataName = _methodInfo.Name;
            var lastDot = metadataName.LastIndexOf('.');
            if (lastDot <= 0)
            {
                _explicitInterfaceImplementations = ImmutableArray<IMethodSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var ifaceMetadataName = metadataName.Substring(0, lastDot);     // "Namespace.IFoo`1"
            var memberName = metadataName.Substring(lastDot + 1);           // "Bar" or "get_Bar"

            var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

            foreach (var iface in declaringType.GetInterfaces())
            {
                // Compare to generic *definition* name, because the metadata prefix uses that
                var candidateName = GetFormattedTypeName(iface);
                if (!string.Equals(candidateName, ifaceMetadataName, StringComparison.Ordinal))
                    continue;

                // We have the right interface. Now find the matching method on that interface.
                foreach (var ifaceMethod in iface.GetMethods(
                             BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
                {
                    if (!string.Equals(ifaceMethod.Name, memberName, StringComparison.Ordinal))
                        continue;

                    if (!HasSameSignature((MethodInfo)_methodInfo, ifaceMethod))
                        continue;

                    // Map interface MethodInfo -> IMethodSymbol via TypeResolver
                    var ifaceMethodSymbol = _typeResolver.ResolveMethodSymbol(ifaceMethod);
                    if (ifaceMethodSymbol is not null)
                        builder.Add(ifaceMethodSymbol);
                }
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    private static string GetFormattedTypeName(Type t)
    {
        // For generic constructed interfaces, we want the definition name
        if (t.IsGenericType && !t.IsGenericTypeDefinition)
            t = t.GetGenericTypeDefinition();

        // Metadata-style full name (includes `1, + for nested, etc.)
        var name = t.FullName ?? t.Name;
        var i = name.LastIndexOf('`');
        if (i > -1)
        {
            name = name[..i];
            var param = t.GetGenericArguments().Select(x => x.Name);
            name = $"{name}<{string.Join(",", param)}>";
        }

        return name;

        /*
        // For generic constructed interfaces, we want the definition name
        if (t.IsGenericType && !t.IsGenericTypeDefinition)
            t = t.GetGenericTypeDefinition();

        // Metadata-style full name (includes `1, + for nested, etc.)
        return t.FullName ?? t.Name;
        */
    }

    private static bool HasSameSignature(MethodInfo impl, MethodInfo iface)
    {
        // Generic arity
        if (impl.IsGenericMethod != iface.IsGenericMethod)
            return false;

        if (impl.IsGenericMethod &&
            impl.GetGenericArguments().Length != iface.GetGenericArguments().Length)
            return false;

        var implParams = impl.GetParameters();
        var ifaceParams = iface.GetParameters();

        if (implParams.Length != ifaceParams.Length)
            return false;

        for (int i = 0; i < implParams.Length; i++)
        {
            var pImpl = implParams[i].ParameterType;
            var pIface = ifaceParams[i].ParameterType;

            if (!TypesEqual(pImpl, pIface))
                return false;
        }

        return TypesEqual(impl.ReturnType, iface.ReturnType);
    }

    private static bool TypesEqual(Type a, Type b)
    {
        // In MetadataLoadContext, Type objects from the same context are comparable by reference
        return a == b;
    }

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
