using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

class ImportBinder : Binder
{
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

        var type = _typeImports.FirstOrDefault(x => x.Name == name);
        if (type is not null)
            return type;

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupType(name);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_aliases.TryGetValue(name, out var symbols))
            return symbols.FirstOrDefault();

        var matchingType = _typeImports.FirstOrDefault(x => x.Name == name);
        if (matchingType is not null)
            return matchingType;

        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            var t = ns.LookupType(name);
            if (t != null)
                return t;
        }

        return ParentBinder?.LookupSymbol(name);
    }

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

        // Members from namespace or type-scope imports (including static members of imported types)
        foreach (var ns in _namespaceOrTypeScopeImports)
        {
            foreach (var member in ns.GetMembers(name))
                if (seen.Add(member))
                    results.Add(member);

            var t = ns.LookupType(name);
            if (t != null && seen.Add(t))
                results.Add(t);
        }

        // Types explicitly imported
        foreach (var type in _typeImports)
            if (type.Name == name && seen.Add(type))
                results.Add(type);

        if (results.Count > 0)
            return results;

        return ParentBinder?.LookupSymbols(name) ?? Enumerable.Empty<ISymbol>();
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);

        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var method in GetExtensionMethodsFromScope(scope, name, receiverType, includePartialMatches))
                if (seen.Add(method))
                    yield return method;
        }

        foreach (var type in _typeImports)
        {
            foreach (var method in GetExtensionMethodsFromScope(type, name, receiverType, includePartialMatches))
                if (seen.Add(method))
                    yield return method;
        }

        foreach (var method in base.LookupExtensionMethods(name, receiverType, includePartialMatches))
            if (seen.Add(method))
                yield return method;
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var seen = new HashSet<IPropertySymbol>(SymbolEqualityComparer.Default);

        foreach (var scope in _namespaceOrTypeScopeImports)
        {
            foreach (var property in GetExtensionPropertiesFromScope(scope, name, receiverType, includePartialMatches))
                if (seen.Add(property))
                    yield return property;
        }

        foreach (var type in _typeImports)
        {
            foreach (var property in GetExtensionPropertiesFromScope(type, name, receiverType, includePartialMatches))
                if (seen.Add(property))
                    yield return property;
        }

        foreach (var property in base.LookupExtensionProperties(name, receiverType, includePartialMatches))
            if (seen.Add(property))
                yield return property;
    }
}
