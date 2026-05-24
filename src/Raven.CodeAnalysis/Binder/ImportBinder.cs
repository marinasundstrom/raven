using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IMethodSymbol>>> _extensionMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IPropertySymbol>>> _extensionPropertyCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IMethodSymbol>>> _extensionStaticMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<IPropertySymbol>>> _extensionStaticPropertyCache = new();
    private readonly ConcurrentDictionary<string, Lazy<ImmutableArray<IMethodSymbol>>> _extensionMethodNameCache = new();
    private readonly IReadOnlyList<INamespaceOrTypeSymbol> _namespaceOrTypeScopeImports;
    private readonly IReadOnlyList<ITypeSymbol> _typeImports;
    private readonly IReadOnlyDictionary<string, IReadOnlyList<IAliasSymbol>> _aliases;

    public ImportBinder(
        Binder parent,
        IReadOnlyList<INamespaceOrTypeSymbol> namespaceOrTypeScopeImports,
        IReadOnlyList<ITypeSymbol> typeImports,
        IReadOnlyDictionary<string, IReadOnlyList<IAliasSymbol>> aliases)
        : base(parent)
    {
        _namespaceOrTypeScopeImports = namespaceOrTypeScopeImports;
        _typeImports = typeImports;
        _aliases = aliases;
    }

    public override ITypeSymbol? LookupType(string name)
    {
        if (_aliases.TryGetValue(name, out var aliasSymbols))
            return aliasSymbols.OfType<ITypeSymbol>().FirstOrDefault();

        var declared = !Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
            ? CurrentNamespace?.LookupType(name)
            : null;
        if (declared is not null)
            return declared;

        var type = _typeImports.FirstOrDefault(x => x.Name == name);
        if (type is not null)
            return type;

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = !Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
                ? ns.LookupType(name)
                : null;
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupType(name);
    }

    public override ISymbol? LookupSymbol(string name)
        => LookupSymbols(name).FirstOrDefault();

    public override INamespaceSymbol? LookupNamespace(string name)
    {
        if (_aliases.TryGetValue(name, out var aliasSymbols))
            return aliasSymbols.OfType<INamespaceSymbol>().FirstOrDefault();

        foreach (var ns in _namespaceOrTypeScopeImports.OfType<INamespaceSymbol>())
        {
            var result = ns.LookupNamespace(name);
            if (result != null)
                return result;
        }

        return ParentBinder?.LookupNamespace(name);
    }

    /// <summary>
    /// Gets namespaces and types whose members are in scope.
    /// </summary>
    public IEnumerable<INamespaceOrTypeSymbol> GetImportedNamespacesOrTypeScopes() => _namespaceOrTypeScopeImports;

    /// <summary>
    /// Get types that have been explicitly imported.
    /// </summary>
    public IEnumerable<ITypeSymbol> GetImportedTypes() => _typeImports;

    /// <summary>
    /// Gets a dictionary with the mapping from alias to resolved symbols.
    /// </summary>
    public IReadOnlyDictionary<string, IReadOnlyList<IAliasSymbol>> GetAliases() => _aliases;

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols;

        var results = new List<ISymbol>();
        var seen = new HashSet<ISymbol>();

        // Members from namespace or type-scope imports. Type-scope imports make
        // static members and nested types/cases available, but never instance
        // members; receiver-based lookup is responsible for instance members.
        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            foreach (var member in GetMembersByName(ns, name))
                if (seen.Add(member))
                    results.Add(member);

            var t = !Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
                ? ns.LookupType(name)
                : null;
            if (t != null && seen.Add(t))
                results.Add(t);

            if (ns is ITypeSymbol importedType)
            {
                foreach (var method in LookupExtensionStaticMethods(name, importedType))
                    if (seen.Add(method))
                        results.Add(method);

                foreach (var property in LookupExtensionStaticProperties(name, importedType))
                    if (seen.Add(property))
                        results.Add(property);
            }
            else if (ns is INamespaceSymbol namespaceSymbol &&
                TryResolveTypeFromNamespaceName(namespaceSymbol, out var namespaceNamedType))
            {
                foreach (var member in GetMembersByName(namespaceNamedType, name))
                    if (seen.Add(member))
                        results.Add(member);

                foreach (var method in LookupExtensionStaticMethods(name, namespaceNamedType))
                    if (seen.Add(method))
                        results.Add(method);

                foreach (var property in LookupExtensionStaticProperties(name, namespaceNamedType))
                    if (seen.Add(property))
                        results.Add(property);
            }
        }

        // Types explicitly imported
        foreach (var type in _typeImports)
            if (type.Name == name && seen.Add(type))
                results.Add(type);

        if (results.Count > 0)
            return results;

        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<ISymbol>();
    }

    private IEnumerable<ISymbol> GetMembersByName(INamespaceOrTypeSymbol symbol, string name)
    {
        IEnumerable<ISymbol> members = symbol.GetMembers(name);
        if (symbol is ITypeSymbol)
            members = members.Where(IsImportableTypeScopeMember);

        foreach (var member in members)
            yield return member;

        if (symbol is INamedTypeSymbol typeSymbol &&
            typeSymbol.TryGetUnion() is { } union)
        {
            foreach (var caseType in union.DeclaredCaseTypes)
            {
                if (string.Equals(caseType.Name, name, StringComparison.Ordinal))
                    yield return caseType;
            }
        }

        if (symbol is INamespaceSymbol namespaceSymbol)
        {
            var includeNamespaceMembers = Compilation.Options.AllowNamespaceMembers &&
                                          Compilation.Options.AllowNamespaceMemberImports;
            foreach (var member in Compilation.GetNamespaceMembers(namespaceSymbol, name, includeNamespaceMembers))
                yield return member;
        }

        yield break;
    }

    internal static bool IsImportableTypeScopeMember(ISymbol member)
        => member switch
        {
            INamespaceOrTypeSymbol => true,
            IFieldSymbol field => field.IsStatic || field.IsConst,
            IPropertySymbol property => property.IsStatic,
            IEventSymbol @event => @event.IsStatic,
            IMethodSymbol method => method.IsStatic && !method.IsConstructor,
            _ => false
        };

    internal bool TryResolveTypeFromNamespaceName(INamespaceSymbol namespaceSymbol, out ITypeSymbol type)
    {
        var namespaceName = namespaceSymbol.ToString();
        if (string.IsNullOrWhiteSpace(namespaceName))
        {
            type = null!;
            return false;
        }

        type = ResolveMetadataType(namespaceName)
            ?? ResolveTypeFromContainingNamespace(namespaceName);
        return type is not null;
    }

    private INamedTypeSymbol? ResolveMetadataType(string name)
        => Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
            ? Compilation.SymbolLookup.GetTypeByMetadataNameMetadataOnly(name)
            : Compilation.SymbolLookup.GetTypeByMetadataNameSourceFirst(name);

    private ITypeSymbol? ResolveTypeFromContainingNamespace(string name)
    {
        var lastDot = name.LastIndexOf('.');
        if (lastDot <= 0 || lastDot == name.Length - 1)
            return null;

        var namespaceName = name[..lastDot];
        var typeName = name[(lastDot + 1)..];
        var ns = Compilation.SymbolLookup.GetNamespace(namespaceName);
        return (!Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed ? Compilation.SymbolLookup.LookupTypeSourceFirst(ns, typeName) : null)
            ?? ns?.GetMembers(typeName).OfType<ITypeSymbol>().FirstOrDefault();
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var methods = CanCacheExtensionLookupResults
            ? _extensionMethodCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<IMethodSymbol>>(
                    () => LookupExtensionMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionMethodsCore(name, receiverType, includePartialMatches);

        foreach (var method in methods)
            yield return method;
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionMethodsByName(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            yield break;

        var methods = CanCacheExtensionLookupResults
            ? _extensionMethodNameCache.GetOrAdd(
                name,
                static (key, self) => new Lazy<ImmutableArray<IMethodSymbol>>(
                    () => self.LookupExtensionMethodsByNameCore(key),
                    LazyThreadSafetyMode.ExecutionAndPublication),
                this).Value
            : LookupExtensionMethodsByNameCore(name);

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

        if (CanCacheExtensionLookupResults &&
            _extensionMethodCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedMethods))
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

        var properties = CanCacheExtensionLookupResults
            ? _extensionPropertyCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<IPropertySymbol>>(
                    () => LookupExtensionPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionPropertiesCore(name, receiverType, includePartialMatches);

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

        if (CanCacheExtensionLookupResults &&
            _extensionPropertyCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedProperties))
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

        var methods = CanCacheExtensionLookupResults
            ? _extensionStaticMethodCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<IMethodSymbol>>(
                    () => LookupExtensionStaticMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionStaticMethodsCore(name, receiverType, includePartialMatches);

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

        if (CanCacheExtensionLookupResults &&
            _extensionStaticMethodCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedMethods))
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

        var properties = CanCacheExtensionLookupResults
            ? _extensionStaticPropertyCache.GetOrAdd(
                new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
                key => new Lazy<ImmutableArray<IPropertySymbol>>(
                    () => LookupExtensionStaticPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches),
                    LazyThreadSafetyMode.ExecutionAndPublication)).Value
            : LookupExtensionStaticPropertiesCore(name, receiverType, includePartialMatches);

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

        if (CanCacheExtensionLookupResults &&
            _extensionStaticPropertyCache.TryGetValue(new BinderExtensionLookupKey(name, receiverType, includePartialMatches), out var cachedProperties))
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
        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var method in GetExtensionMethodsFromScope(scope, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionMethodDedupKey(method)))
                    builder.Add(method);
            }
        }

        foreach (var type in _typeImports)
        {
            foreach (var method in GetExtensionMethodsFromScope(type, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionMethodDedupKey(method)))
                    builder.Add(method);
            }
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

        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var method in GetExtensionMethodsByNameFromScope(scope, name))
            {
                if (seen.Add(GetExtensionMethodDedupKey(method)))
                    builder.Add(method);
            }
        }

        foreach (var type in _typeImports)
        {
            foreach (var method in GetExtensionMethodsByNameFromScope(type, name))
            {
                if (seen.Add(GetExtensionMethodDedupKey(method)))
                    builder.Add(method);
            }
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
        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var property in GetExtensionPropertiesFromScope(scope, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionPropertyDedupKey(property)))
                    builder.Add(property);
            }
        }

        foreach (var type in _typeImports)
        {
            foreach (var property in GetExtensionPropertiesFromScope(type, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionPropertyDedupKey(property)))
                    builder.Add(property);
            }
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
        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var method in GetExtensionStaticMethodsFromScope(scope, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionMethodDedupKey(method)))
                    builder.Add(method);
            }
        }

        foreach (var type in _typeImports)
        {
            foreach (var method in GetExtensionStaticMethodsFromScope(type, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionMethodDedupKey(method)))
                    builder.Add(method);
            }
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
        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var property in GetExtensionStaticPropertiesFromScope(scope, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionPropertyDedupKey(property)))
                    builder.Add(property);
            }
        }

        foreach (var type in _typeImports)
        {
            foreach (var property in GetExtensionStaticPropertiesFromScope(type, name, receiverType, includePartialMatches))
            {
                if (seen.Add(GetExtensionPropertyDedupKey(property)))
                    builder.Add(property);
            }
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

    protected override IReadOnlyList<INamespaceOrTypeSymbol> GetImportedScopesForTypeResolution()
    {
        var scopes = new List<INamespaceOrTypeSymbol>(capacity: 8);

        // 1) add imported scopes first (highest priority)
        foreach (var s in _namespaceOrTypeScopeImports) // whatever your internal representation is
        {
            if (s is null)
                continue;

            if (!scopes.Contains(s, SymbolEqualityComparer.Default))
                scopes.Add(s);
        }

        // 2) then append the base/parent scopes
        var baseScopes = base.GetImportedScopesForTypeResolution();
        foreach (var s in baseScopes)
        {
            if (!scopes.Contains(s, SymbolEqualityComparer.Default))
                scopes.Add(s);
        }

        return scopes;
    }
}
