using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class PEEventSymbol : PESymbol, IEventSymbol
{
    private readonly ReflectionTypeLoader _reflectionTypeLoader;
    private readonly EventInfo _eventInfo;
    private ITypeSymbol? _type;
    private Accessibility? _accessibility;
    private ImmutableArray<IEventSymbol>? _explicitInterfaceImplementations;
    private string? _name;

    public PEEventSymbol(ReflectionTypeLoader reflectionTypeLoader, EventInfo eventInfo, INamedTypeSymbol? containingType, Location[] locations)
        : base(containingType, containingType, containingType.ContainingNamespace, locations)
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _eventInfo = eventInfo;
    }

    public override SymbolKind Kind => SymbolKind.Event;

    public override string Name
    {
        get
        {
            if (_name is not null)
            {
                return _name;
            }

            if (_eventInfo.Name.Contains('.'))
            {
                _name = _eventInfo.Name.Split('.').Last();
            }
            else
            {
                _name = _eventInfo.Name;
            }

            return _name;
        }
    }

    public ITypeSymbol Type => _type ??= _reflectionTypeLoader.ResolveType(_eventInfo) ?? _reflectionTypeLoader.ResolveType(_eventInfo.EventHandlerType!);

    public IMethodSymbol? AddMethod { get; set; }

    public IMethodSymbol? RemoveMethod { get; set; }

    public override Accessibility DeclaredAccessibility => _accessibility ??= MapAccessibility(_eventInfo);

    public override bool IsStatic => (_eventInfo.AddMethod?.IsStatic ?? false) || (_eventInfo.RemoveMethod?.IsStatic ?? false);

    public ImmutableArray<IEventSymbol> ExplicitInterfaceImplementations
    {
        get
        {
            if (_explicitInterfaceImplementations.HasValue)
                return _explicitInterfaceImplementations.Value;

            var declaringType = _eventInfo.DeclaringType;
            if (declaringType is null)
            {
                _explicitInterfaceImplementations = ImmutableArray<IEventSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var accessor = _eventInfo.AddMethod ?? _eventInfo.RemoveMethod;
            if (accessor is null)
            {
                _explicitInterfaceImplementations = ImmutableArray<IEventSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var metadataName = accessor.Name;

            if (!metadataName.Contains('.'))
            {
                _explicitInterfaceImplementations = ImmutableArray<IEventSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var lastDot = metadataName.LastIndexOf('.');
            if (lastDot <= 0)
            {
                _explicitInterfaceImplementations = ImmutableArray<IEventSymbol>.Empty;
                return _explicitInterfaceImplementations.Value;
            }

            var ifaceMetadataName = metadataName.Substring(0, lastDot);
            var ifaceAccessorName = metadataName.Substring(lastDot + 1);

            var builder = ImmutableArray.CreateBuilder<IEventSymbol>();

            foreach (var iface in declaringType.GetInterfaces())
            {
                var candidateName = GetFormattedTypeName(iface);
                if (!string.Equals(candidateName, ifaceMetadataName, StringComparison.Ordinal))
                    continue;

                foreach (var ifaceEvent in iface.GetEvents(
                             BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance))
                {
                    var ifaceAdd = ifaceEvent.AddMethod;
                    var ifaceRemove = ifaceEvent.RemoveMethod;

                    if (ifaceAdd is null && ifaceRemove is null)
                        continue;

                    bool accessorNameMatches =
                        (ifaceAdd != null && string.Equals(ifaceAdd.Name, ifaceAccessorName, StringComparison.Ordinal)) ||
                        (ifaceRemove != null && string.Equals(ifaceRemove.Name, ifaceAccessorName, StringComparison.Ordinal));

                    if (!accessorNameMatches)
                        continue;

                    if (!EventSignaturesMatch(_eventInfo, ifaceEvent))
                        continue;

                    var ifaceEventSymbol = _reflectionTypeLoader.ResolveEventSymbol(ifaceEvent);
                    if (ifaceEventSymbol is not null)
                        builder.Add(ifaceEventSymbol);
                }
            }

            _explicitInterfaceImplementations = builder.ToImmutable();
            return _explicitInterfaceImplementations.Value;
        }
    }

    private static string GetFormattedTypeName(Type t)
    {
        if (t.IsGenericType && !t.IsGenericTypeDefinition)
            t = t.GetGenericTypeDefinition();

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

    private static bool EventSignaturesMatch(EventInfo impl, EventInfo iface)
    {
        return TypesEqual(impl.EventHandlerType, iface.EventHandlerType);
    }

    private static bool TypesEqual(Type? a, Type? b)
    {
        return PEReflectionTypeIdentity.AreEquivalent(a, b);
    }

    public EventInfo GetEventInfo()
    {
        return _eventInfo;
    }

}
