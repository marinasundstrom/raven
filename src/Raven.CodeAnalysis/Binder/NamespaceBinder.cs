using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IMethodSymbol>>> _extensionMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IPropertySymbol>>> _extensionPropertyCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IMethodSymbol>>> _extensionStaticMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IPropertySymbol>>> _extensionStaticPropertyCache = new();
    private readonly ConcurrentDictionary<string, Lazy<ImmutableArray<IMethodSymbol>>> _extensionMethodNameCache = new();
    private readonly INamespaceSymbol _namespaceSymbol;
    private readonly List<SourceNamedTypeSymbol> _declaredTypes = [];

    public NamespaceBinder(Binder parent, INamespaceSymbol ns)
        : base(parent)
    {
        _namespaceSymbol = ns;
    }

    public override INamespaceSymbol? CurrentNamespace => _namespaceSymbol;

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        if (node is BaseNamespaceDeclarationSyntax)
            return _namespaceSymbol;

        return base.BindDeclaredSymbol(node);
    }

    public void DeclareType(SourceNamedTypeSymbol type)
    {
        _declaredTypes.Add(type);
    }

    public IEnumerable<SourceNamedTypeSymbol> DeclaredTypes => _declaredTypes;

    public INamespaceSymbol NamespaceSymbol => _namespaceSymbol;

    public override IEnumerable<IMethodSymbol> LookupExtensionMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var methods = _extensionMethodCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => new Lazy<ImmutableArray<IMethodSymbol>>(
                () => LookupExtensionMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;

        foreach (var method in methods)
            yield return method;
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionMethodsByName(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            yield break;

        var methods = _extensionMethodNameCache.GetOrAdd(
            name,
            static (key, self) => new Lazy<ImmutableArray<IMethodSymbol>>(
                () => self.LookupExtensionMethodsByNameCore(key),
                LazyThreadSafetyMode.ExecutionAndPublication),
            this).Value;

        foreach (var method in methods)
            yield return method;
    }

    public override bool TryGetCachedExtensionMethods(
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<IMethodSymbol> methods)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            methods = ImmutableArray<IMethodSymbol>.Empty;
            return false;
        }

        if (_extensionMethodCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedMethods))
        {
            methods = cachedMethods.Value;
            return true;
        }

        return base.TryGetCachedExtensionMethods(name, receiverType, includePartialMatches, out methods);
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var properties = _extensionPropertyCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => new Lazy<ImmutableArray<IPropertySymbol>>(
                () => LookupExtensionPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;

        foreach (var property in properties)
            yield return property;
    }

    public override bool TryGetCachedExtensionProperties(
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<IPropertySymbol> properties)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            properties = ImmutableArray<IPropertySymbol>.Empty;
            return false;
        }

        if (_extensionPropertyCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedProperties))
        {
            properties = cachedProperties.Value;
            return true;
        }

        return base.TryGetCachedExtensionProperties(name, receiverType, includePartialMatches, out properties);
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionStaticMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var methods = _extensionStaticMethodCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => new Lazy<ImmutableArray<IMethodSymbol>>(
                () => LookupExtensionStaticMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;

        foreach (var method in methods)
            yield return method;
    }

    public override bool TryGetCachedExtensionStaticMethods(
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<IMethodSymbol> methods)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            methods = ImmutableArray<IMethodSymbol>.Empty;
            return false;
        }

        if (_extensionStaticMethodCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedMethods))
        {
            methods = cachedMethods.Value;
            return true;
        }

        return base.TryGetCachedExtensionStaticMethods(name, receiverType, includePartialMatches, out methods);
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionStaticProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var properties = _extensionStaticPropertyCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => new Lazy<ImmutableArray<IPropertySymbol>>(
                () => LookupExtensionStaticPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                LazyThreadSafetyMode.ExecutionAndPublication)).Value;

        foreach (var property in properties)
            yield return property;
    }

    public override bool TryGetCachedExtensionStaticProperties(
        string? name,
        ITypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<IPropertySymbol> properties)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            properties = ImmutableArray<IPropertySymbol>.Empty;
            return false;
        }

        if (_extensionStaticPropertyCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedProperties))
        {
            properties = cachedProperties.Value;
            return true;
        }

        return base.TryGetCachedExtensionStaticProperties(name, receiverType, includePartialMatches, out properties);
    }

    private ImmutableArray<IMethodSymbol> LookupExtensionMethodsCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in GetExtensionMethodsFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionMethodDedupKey(method)))
                builder.Add(method);
        }

        foreach (var method in base.LookupExtensionMethods(name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionMethodDedupKey(method)))
                builder.Add(method);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IMethodSymbol> LookupExtensionMethodsByNameCore(string name)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in GetExtensionMethodsByNameFromScope(_namespaceSymbol, name))
        {
            if (seen.Add(GetExtensionMethodDedupKey(method)))
                builder.Add(method);
        }

        foreach (var method in base.LookupExtensionMethodsByName(name))
        {
            if (seen.Add(GetExtensionMethodDedupKey(method)))
                builder.Add(method);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IPropertySymbol> LookupExtensionPropertiesCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

        foreach (var property in GetExtensionPropertiesFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionPropertyDedupKey(property)))
                builder.Add(property);
        }

        foreach (var property in base.LookupExtensionProperties(name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionPropertyDedupKey(property)))
                builder.Add(property);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IMethodSymbol> LookupExtensionStaticMethodsCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in GetExtensionStaticMethodsFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionMethodDedupKey(method)))
                builder.Add(method);
        }

        foreach (var method in base.LookupExtensionStaticMethods(name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionMethodDedupKey(method)))
                builder.Add(method);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IPropertySymbol> LookupExtensionStaticPropertiesCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

        foreach (var property in GetExtensionStaticPropertiesFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionPropertyDedupKey(property)))
                builder.Add(property);
        }

        foreach (var property in base.LookupExtensionStaticProperties(name, receiverType, includePartialMatches))
        {
            if (seen.Add(GetExtensionPropertyDedupKey(property)))
                builder.Add(property);
        }

        return builder.ToImmutable();
    }

    private static string GetExtensionMethodDedupKey(IMethodSymbol method)
        => method.GetShallowLookupIdentityKey();

    private static string GetExtensionPropertyDedupKey(IPropertySymbol property)
        => property.GetShallowLookupIdentityKey();
}
