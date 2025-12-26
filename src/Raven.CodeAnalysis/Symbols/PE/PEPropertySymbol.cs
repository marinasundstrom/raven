using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PEPropertySymbol : PESymbol, IPropertySymbol
{
    private readonly TypeResolver _typeResolver;
    private readonly PropertyInfo _propertyInfo;
    private ITypeSymbol _type;
    private Accessibility? _accessibility;
    private ImmutableArray<IPropertySymbol>? _explicitInterfaceImplementations;
    private string? _name;
    private string? _extensionMarkerName;

    public PEPropertySymbol(TypeResolver typeResolver, PropertyInfo propertyInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _typeResolver = typeResolver;
        _propertyInfo = propertyInfo;
    }

    public override SymbolKind Kind => SymbolKind.Property;

    public override string Name
    {
        get
        {
            if (_name is not null)
            {
                return _name;
            }
            if (_propertyInfo.Name.Contains('.'))
            {
                _name = _propertyInfo.Name.Split('.').Last();
            }
            else
            {
                _name = _propertyInfo.Name;
            }
            return _name;
        }
    }

    public ITypeSymbol Type
    {
        get
        {
            return _type ??= _typeResolver.ResolveType(_propertyInfo);
        }
    }

    public IMethodSymbol? GetMethod { get; set; }
    public IMethodSymbol? SetMethod { get; set; }

    public IPropertySymbol OriginalDefinition { get; }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_propertyInfo);

    public override bool IsStatic => (_propertyInfo.GetMethod?.IsStatic ?? false) || (_propertyInfo.SetMethod?.IsStatic ?? false);

    public bool IsIndexer => _propertyInfo.GetIndexParameters().Length > 0;

    public ImmutableArray<IPropertySymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var declaringType = _propertyInfo.DeclaringType;
            if (declaringType is null)
            {
                _explicitInterfaceImplementations = ImmutableArray<IPropertySymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            // Use one of the accessors to detect explicitness and parse the interface name
            var accessor = _propertyInfo.GetMethod ?? _propertyInfo.SetMethod;
            if (accessor is null)
            {
                _explicitInterfaceImplementations = ImmutableArray<IPropertySymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            // Metadata accessor name is something like "Namespace.IFoo`1.get_Value"
            var metadataName = accessor.Name;

            // Fast-path: non-explicit properties have accessor name without a dot
            if (!metadataName.Contains('.'))
            {
                _explicitInterfaceImplementations = ImmutableArray<IPropertySymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var lastDot = metadataName.LastIndexOf('.');
            if (lastDot <= 0)
            {
                _explicitInterfaceImplementations = ImmutableArray<IPropertySymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var ifaceMetadataName = metadataName.Substring(0, lastDot);   // "Namespace.IFoo`1"
            var ifaceAccessorName = metadataName.Substring(lastDot + 1);  // "get_Value" / "set_Value"

            var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

            foreach (var iface in declaringType.GetInterfaces())
            {
                // Compare to generic *definition* name, because explicit impl prefix uses that
                var candidateName = GetFormattedTypeName(iface);
                if (!string.Equals(candidateName, ifaceMetadataName, StringComparison.Ordinal))
                    continue;

                // We have the right interface. Now find the matching property on that interface.
                foreach (var ifaceProp in iface.GetProperties(
                             BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
                {
                    var ifaceGet = ifaceProp.GetMethod;
                    var ifaceSet = ifaceProp.SetMethod;

                    if (ifaceGet is null && ifaceSet is null)
                        continue;

                    bool accessorNameMatches =
                        (ifaceGet != null && string.Equals(ifaceGet.Name, ifaceAccessorName, StringComparison.Ordinal)) ||
                        (ifaceSet != null && string.Equals(ifaceSet.Name, ifaceAccessorName, StringComparison.Ordinal));

                    if (!accessorNameMatches)
                        continue;

                    if (!PropertySignaturesMatch(_propertyInfo, ifaceProp))
                        continue;

                    var ifacePropSymbol = _typeResolver.ResolvePropertySymbol(ifaceProp);
                    if (ifacePropSymbol is not null)
                        builder.Add(ifacePropSymbol);
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
    }

    private static bool PropertySignaturesMatch(PropertyInfo impl, PropertyInfo iface)
    {
        // Property type
        if (!TypesEqual(impl.PropertyType, iface.PropertyType))
            return false;

        // Indexer parameters
        var implIdx = impl.GetIndexParameters();
        var ifaceIdx = iface.GetIndexParameters();

        if (implIdx.Length != ifaceIdx.Length)
            return false;

        for (int i = 0; i < implIdx.Length; i++)
        {
            if (!TypesEqual(implIdx[i].ParameterType, ifaceIdx[i].ParameterType))
                return false;
        }

        return true;
    }

    private static bool TypesEqual(Type a, Type b)
    {
        // In MetadataLoadContext, Type instances from the same context are comparable by reference.
        return a == b;
    }

    public PropertyInfo GetPropertyInfo()
    {
        return _propertyInfo;
    }

    internal bool TryGetExtensionMarkerName(out string markerName)
    {
        markerName = _extensionMarkerName ?? string.Empty;

        if (_extensionMarkerName is not null)
            return markerName.Length > 0;

        try
        {
            foreach (var attribute in _propertyInfo.GetCustomAttributesData())
            {
                if (attribute.AttributeType.FullName != "System.Runtime.CompilerServices.ExtensionMarkerNameAttribute")
                    continue;

                if (attribute.ConstructorArguments is [{ Value: string name }])
                {
                    _extensionMarkerName = name;
                    markerName = name;
                    return true;
                }
            }
        }
        catch (Exception)
        {
            _extensionMarkerName = string.Empty;
            return false;
        }

        _extensionMarkerName = string.Empty;
        return false;
    }
}
