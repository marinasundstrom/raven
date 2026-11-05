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
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<MethodSymbol>>> _extensionMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<PropertySymbol>>> _extensionPropertyCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<MethodSymbol>>> _extensionStaticMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, Lazy<ImmutableArray<PropertySymbol>>> _extensionStaticPropertyCache = new();
    private readonly ConcurrentDictionary<string, Lazy<ImmutableArray<MethodSymbol>>> _extensionMethodNameCache = new();
    private readonly IReadOnlyList<NamespaceOrTypeSymbol> _namespaceOrTypeScopeImports;
    private readonly IReadOnlyList<TypeSymbol> _typeImports;
    private readonly IReadOnlyDictionary<string, IReadOnlyList<AliasSymbol>> _aliases;

    public ImportBinder(
        Binder parent,
        IReadOnlyList<NamespaceOrTypeSymbol> namespaceOrTypeScopeImports,
        IReadOnlyList<TypeSymbol> typeImports,
        IReadOnlyDictionary<string, IReadOnlyList<AliasSymbol>> aliases)
        : base(parent)
    {
        _namespaceOrTypeScopeImports = namespaceOrTypeScopeImports;
        _typeImports = typeImports;
        _aliases = aliases;
    }

    public override TypeSymbol? LookupType(string name)
    {
        if (_aliases.TryGetValue(name, out var aliasSymbols))
            return aliasSymbols.OfType<TypeSymbol>().FirstOrDefault();

        var declared = LookupTypeInScope(CurrentNamespace, name);
        if (declared is not null)
            return declared;

        var type = _typeImports.FirstOrDefault(x => IsImportedTypeName(x, name));
        if (type is not null)
            return type;

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = LookupTypeInScope(ns, name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupType(name);
    }

    public override Symbol? LookupSymbol(string name)
        => LookupSymbols(name).FirstOrDefault();

    public override NamespaceSymbol? LookupNamespace(string name)
    {
        if (_aliases.TryGetValue(name, out var aliasSymbols))
            return aliasSymbols.OfType<NamespaceSymbol>().FirstOrDefault();

        foreach (var ns in _namespaceOrTypeScopeImports.OfType<NamespaceSymbol>())
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
    public IEnumerable<NamespaceOrTypeSymbol> GetImportedNamespacesOrTypeScopes() => _namespaceOrTypeScopeImports;

    /// <summary>
    /// Get types that have been explicitly imported.
    /// </summary>
    public IEnumerable<TypeSymbol> GetImportedTypes() => _typeImports;

    /// <summary>
    /// Gets a dictionary with the mapping from alias to resolved symbols.
    /// </summary>
    public IReadOnlyDictionary<string, IReadOnlyList<AliasSymbol>> GetAliases() => _aliases;

    private TypeSymbol? LookupTypeInScope(NamespaceOrTypeSymbol? scope, string name)
    {
        if (scope is null)
            return null;

        if (Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed &&
            scope is NamespaceSymbol namespaceScope &&
            Compilation.TryDeclareIndexedSourceType(namespaceScope, name, out var sourceType))
        {
            return sourceType;
        }

        if (Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed &&
            scope is SourceNamespaceSymbol sourceNamespace)
        {
            return sourceNamespace.LookupTypeDeclared(name);
        }

        var type = scope.LookupType(name);
        return scope is NamespaceSymbol && type is NamedTypeSymbol named &&
            !SymbolEqualityComparer.Default.Equals(named.ContainingAssembly, Compilation.Assembly)
            ? Compilation.GetTypeByMetadataName(named.ToFullyQualifiedMetadataName())
            : type;
    }

    public override IEnumerable<Symbol> LookupSymbols(string name)
    {
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols;

        // Local types hide imported types, but not imported static factories.
        // Check only type-scope values before returning the local type family;
        // unrelated namespace imports must not force declaration completion.
        if (LookupTypeInScope(CurrentNamespace, name) is { } declaredType)
        {
            var importedValues = new List<Symbol>();
            foreach (var importedType in _namespaceOrTypeScopeImports.OfType<TypeSymbol>())
            {
                importedValues.AddRange(GetMembersByName(importedType, name)
                    .Where(member => member is not NamespaceOrTypeSymbol));
                importedValues.AddRange(LookupExtensionStaticMethods(name, importedType));
                importedValues.AddRange(LookupExtensionStaticProperties(name, importedType));
            }

            if (importedValues.Count > 0)
                return importedValues.Distinct(SymbolEqualityComparer.Default);

            return new Symbol[] { declaredType }
                .Concat(GetMembersByName(CurrentNamespace!, name).OfType<TypeSymbol>())
                .Distinct(SymbolEqualityComparer.Default);
        }

        var results = new List<Symbol>();
        var seen = new HashSet<Symbol>();

        // Members from namespace or type-scope imports. Type-scope imports make
        // static members and nested types/cases available, but never instance
        // members; receiver-based lookup is responsible for instance members.
        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            foreach (var member in GetMembersByName(ns, name))
                if (seen.Add(member))
                    results.Add(member);

            var t = LookupTypeInScope(ns, name);
            if (t != null && seen.Add(t))
                results.Add(t);

            if (ns is TypeSymbol importedType)
            {
                foreach (var method in LookupExtensionStaticMethods(name, importedType))
                    if (seen.Add(method))
                        results.Add(method);

                foreach (var property in LookupExtensionStaticProperties(name, importedType))
                    if (seen.Add(property))
                        results.Add(property);
            }
            else if (ns is NamespaceSymbol namespaceSymbol &&
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
            if (IsImportedTypeName(type, name) && seen.Add(type))
                results.Add(type);

        if (results.Count > 0)
            return results;

        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<Symbol>();
    }

    private IEnumerable<Symbol> GetMembersByName(NamespaceOrTypeSymbol symbol, string name)
    {
        IEnumerable<Symbol> members = symbol.GetMembers(name);
        if (symbol is TypeSymbol)
            members = members.Where(IsImportableTypeScopeMember);

        foreach (var member in members)
            if (CanonicalMember(member) is { } selected)
                yield return selected;

        if (symbol is NamedTypeSymbol typeSymbol &&
            typeSymbol.TryGetUnion() is { } union)
        {
            foreach (var caseType in union.DeclaredCaseTypes)
            {
                if (string.Equals(caseType.Name, name, StringComparison.Ordinal))
                    yield return caseType;
            }
        }

        if (symbol is NamespaceSymbol namespaceSymbol)
        {
            var includeNamespaceMembers = Compilation.Options.AllowNamespaceMembers &&
                                          Compilation.Options.AllowNamespaceMemberImports;
            foreach (var member in Compilation.GetNamespaceMembers(namespaceSymbol, name, includeNamespaceMembers))
                if (CanonicalMember(member) is { } selected)
                    yield return selected;
        }

        yield break;

        Symbol? CanonicalMember(Symbol member)
            => symbol is NamespaceSymbol && member is NamedTypeSymbol type &&
               !SymbolEqualityComparer.Default.Equals(type.ContainingAssembly, Compilation.Assembly)
                ? Compilation.GetTypeByMetadataName(type.ToFullyQualifiedMetadataName())
                : member;
    }

    private static bool IsImportedTypeName(TypeSymbol type, string name)
    {
        if (string.Equals(type.Name, name, StringComparison.Ordinal))
            return true;

        if (type is NamedTypeSymbol named)
        {
            var metadataName = named.MetadataName;
            var tickIndex = metadataName.IndexOf('`');
            if (tickIndex > 0 &&
                string.Equals(metadataName[..tickIndex], name, StringComparison.Ordinal))
            {
                return true;
            }
        }

        return false;
    }

    internal static bool IsImportableTypeScopeMember(Symbol member)
        => member switch
        {
            NamespaceOrTypeSymbol => true,
            FieldSymbol field => field.IsStatic || field.IsConst,
            PropertySymbol property => property.IsStatic,
            EventSymbol @event => @event.IsStatic,
            MethodSymbol method => method.IsStatic && !method.IsConstructor,
            _ => false
        };

    internal bool TryResolveTypeFromNamespaceName(NamespaceSymbol namespaceSymbol, out TypeSymbol type)
    {
        var namespaceName = namespaceSymbol.ToString();
        if (string.IsNullOrEmpty(namespaceName))
        {
            type = null!;
            return false;
        }

        type = ResolveMetadataType(namespaceName)
            ?? ResolveTypeFromContainingNamespace(namespaceName);

        if (type is UnionCompanionSymbol companion &&
            companion.TryGetAssociatedUnion(out var union))
        {
            type = (TypeSymbol)union;
        }

        return type is not null;
    }

    private NamedTypeSymbol? ResolveMetadataType(string name)
        => Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
            ? Compilation.SymbolLookup.GetTypeByMetadataNameMetadataOnly(name)
            : Compilation.SymbolLookup.GetTypeByMetadataNameSourceFirst(name);

    private TypeSymbol? ResolveTypeFromContainingNamespace(string name)
    {
        var lastDot = name.LastIndexOf('.');
        if (lastDot <= 0 || lastDot == name.Length - 1)
            return null;

        var namespaceName = name[..lastDot];
        var typeName = name[(lastDot + 1)..];
        var ns = Compilation.SymbolLookup.GetNamespace(namespaceName);
        return (!Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed ? Compilation.SymbolLookup.LookupTypeSourceFirst(ns, typeName) : null)
            ?? ns?.GetMembers(typeName).OfType<TypeSymbol>().FirstOrDefault();
    }

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

    private ImmutableArray<MethodSymbol> LookupExtensionMethodsByNameCore(string name)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<MethodSymbol>();

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

    private ImmutableArray<PropertySymbol> LookupExtensionPropertiesCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<PropertySymbol>();
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

    private ImmutableArray<MethodSymbol> LookupExtensionStaticMethodsCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<MethodSymbol>();
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

    private ImmutableArray<PropertySymbol> LookupExtensionStaticPropertiesCore(string? name, TypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        var builder = ImmutableArray.CreateBuilder<PropertySymbol>();
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

    private static string GetExtensionMethodDedupKey(MethodSymbol method)
        => method.GetShallowLookupIdentityKey();

    private static string GetExtensionPropertyDedupKey(PropertySymbol property)
        => property.GetShallowLookupIdentityKey();

    protected override IReadOnlyList<NamespaceOrTypeSymbol> GetImportedScopesForTypeResolution()
    {
        var scopes = new List<NamespaceOrTypeSymbol>(capacity: 8);

        // 1) add imported scopes first (highest priority)
        foreach (var s in _namespaceOrTypeScopeImports) // whatever your internal representation is
        {
            if (s is null)
                continue;

            if (!scopes.Any(existing => ReferenceEquals(existing, s)))
                scopes.Add(s);
        }

        // 2) then append the base/parent scopes
        var baseScopes = base.GetImportedScopesForTypeResolution();
        foreach (var s in baseScopes)
        {
            if (!scopes.Any(existing => ReferenceEquals(existing, s)))
                scopes.Add(s);
        }

        return scopes;
    }
}
