using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<MethodSymbol>>> _extensionMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<PropertySymbol>>> _extensionPropertyCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<MethodSymbol>>> _extensionStaticMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<PropertySymbol>>> _extensionStaticPropertyCache = new();
    private readonly ConcurrentDictionary<string, Lazy<ImmutableArray<MethodSymbol>>> _extensionMethodNameCache = new();
    private readonly NamespaceSymbol _namespaceSymbol;
    private readonly List<SourceNamedTypeSymbol> _declaredTypes = [];

    public NamespaceBinder(Binder parent, NamespaceSymbol ns)
        : base(parent)
    {
        _namespaceSymbol = ns;
    }

    public override NamespaceSymbol? CurrentNamespace => _namespaceSymbol;

    public override Symbol? LookupSymbol(string name)
        => LookupSymbols(name).FirstOrDefault();

    public override IEnumerable<Symbol> LookupSymbols(string name)
    {
        var boundary = ParentBinder?.CurrentNamespace?.ToMetadataName();
        for (NamespaceSymbol? scope = _namespaceSymbol; scope is not null; scope = scope.ContainingNamespace)
        {
            if (LookupTypeInNamespace(scope, name, includeGlobalFallback: false) is { } type)
            {
                yield return type;
                // Preserve same-scope arity alternatives without admitting an
                // unrelated type family from an outer namespace.
                foreach (var sibling in type.ContainingNamespace?.GetTypeMembers(name) ?? [])
                    if (!SymbolEqualityComparer.Default.Equals(sibling, type))
                        yield return sibling;
                yield break;
            }

            if (scope.ToMetadataName() == boundary)
                break;
        }

        foreach (var symbol in base.LookupSymbols(name))
            yield return symbol;
    }

    public override TypeSymbol? LookupType(string name)
    {
        // Dotted namespace declarations need not have a binder for every part.
        // Stop at the parent binder's namespace so its imports run before farther scopes.
        var boundary = ParentBinder?.CurrentNamespace?.ToMetadataName();
        for (NamespaceSymbol? scope = _namespaceSymbol; scope is not null; scope = scope.ContainingNamespace)
        {
            if (LookupTypeInNamespace(scope, name, includeGlobalFallback: false) is { } type)
                return type;
            if (scope.ToMetadataName() == boundary)
                break;
        }
        return ParentBinder?.LookupType(name);
    }

    public override NamespaceSymbol? LookupNamespace(string name)
    {
        var boundary = ParentBinder?.CurrentNamespace?.ToMetadataName();
        for (NamespaceSymbol? scope = _namespaceSymbol; scope is not null; scope = scope.ContainingNamespace)
        {
            if (Compilation.SymbolLookup.LookupNamespaceSourceFirst(scope, name, includeGlobalFallback: false) is { } result)
                return result;
            if (scope.ToMetadataName() == boundary)
                break;
        }
        return ParentBinder?.LookupNamespace(name);
    }

    public override Symbol? BindDeclaredSymbol(SyntaxNode node)
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

    public NamespaceSymbol NamespaceSymbol => _namespaceSymbol;

    public override IEnumerable<MethodSymbol> LookupExtensionMethods(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var methods = CanCacheExtensionLookupResults
            ? _extensionMethodCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<MethodSymbol>>(
                    () => LookupExtensionMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionMethodsCore(name, receiverType, includePartialMatches);

        foreach (var method in methods)
            yield return method;
    }

    public override IEnumerable<MethodSymbol> LookupExtensionMethodsByName(string name)
    {
        if (string.IsNullOrEmpty(name))
            yield break;

        var methods = CanCacheExtensionLookupResults
            ? _extensionMethodNameCache.GetOrAdd(
                name,
                static (key, self) => new Lazy<ImmutableArray<MethodSymbol>>(
                    () => self.LookupExtensionMethodsByNameCore(key),
                    LazyThreadSafetyMode.ExecutionAndPublication),
                this).Value
            : LookupExtensionMethodsByNameCore(name);

        foreach (var method in methods)
            yield return method;
    }

    public override bool TryGetCachedExtensionMethods(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<MethodSymbol> methods)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            methods = ImmutableArray<MethodSymbol>.Empty;
            return false;
        }

        if (CanCacheExtensionLookupResults &&
            _extensionMethodCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedMethods))
        {
            methods = cachedMethods.Value;
            return true;
        }

        return base.TryGetCachedExtensionMethods(name, receiverType, includePartialMatches, out methods);
    }

    public override IEnumerable<PropertySymbol> LookupExtensionProperties(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var properties = CanCacheExtensionLookupResults
            ? _extensionPropertyCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<PropertySymbol>>(
                    () => LookupExtensionPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionPropertiesCore(name, receiverType, includePartialMatches);

        foreach (var property in properties)
            yield return property;
    }

    public override bool TryGetCachedExtensionProperties(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<PropertySymbol> properties)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            properties = ImmutableArray<PropertySymbol>.Empty;
            return false;
        }

        if (CanCacheExtensionLookupResults &&
            _extensionPropertyCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedProperties))
        {
            properties = cachedProperties.Value;
            return true;
        }

        return base.TryGetCachedExtensionProperties(name, receiverType, includePartialMatches, out properties);
    }

    public override IEnumerable<MethodSymbol> LookupExtensionStaticMethods(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var methods = CanCacheExtensionLookupResults
            ? _extensionStaticMethodCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<MethodSymbol>>(
                    () => LookupExtensionStaticMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionStaticMethodsCore(name, receiverType, includePartialMatches);

        foreach (var method in methods)
            yield return method;
    }

    public override bool TryGetCachedExtensionStaticMethods(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<MethodSymbol> methods)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            methods = ImmutableArray<MethodSymbol>.Empty;
            return false;
        }

        if (CanCacheExtensionLookupResults &&
            _extensionStaticMethodCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedMethods))
        {
            methods = cachedMethods.Value;
            return true;
        }

        return base.TryGetCachedExtensionStaticMethods(name, receiverType, includePartialMatches, out methods);
    }

    public override IEnumerable<PropertySymbol> LookupExtensionStaticProperties(string? name, TypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var properties = CanCacheExtensionLookupResults
            ? _extensionStaticPropertyCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<PropertySymbol>>(
                    () => LookupExtensionStaticPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionStaticPropertiesCore(name, receiverType, includePartialMatches);

        foreach (var property in properties)
            yield return property;
    }

    public override bool TryGetCachedExtensionStaticProperties(
        string? name,
        TypeSymbol receiverType,
        bool includePartialMatches,
        out ImmutableArray<PropertySymbol> properties)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
        {
            properties = ImmutableArray<PropertySymbol>.Empty;
            return false;
        }

        if (CanCacheExtensionLookupResults &&
            _extensionStaticPropertyCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedProperties))
        {
            properties = cachedProperties.Value;
            return true;
        }

        return base.TryGetCachedExtensionStaticProperties(name, receiverType, includePartialMatches, out properties);
    }

    private ImmutableArray<MethodSymbol> LookupExtensionMethodsCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<MethodSymbol>();

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

    private ImmutableArray<MethodSymbol> LookupExtensionMethodsByNameCore(string name)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<MethodSymbol>();

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

    private ImmutableArray<PropertySymbol> LookupExtensionPropertiesCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<PropertySymbol>();

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

    private ImmutableArray<MethodSymbol> LookupExtensionStaticMethodsCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<MethodSymbol>();

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

    private ImmutableArray<PropertySymbol> LookupExtensionStaticPropertiesCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<PropertySymbol>();

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

    private static string GetExtensionMethodDedupKey(MethodSymbol method)
        => method.GetShallowLookupIdentityKey();

    private static string GetExtensionPropertyDedupKey(PropertySymbol property)
        => property.GetShallowLookupIdentityKey();
}
