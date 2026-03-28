using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class NamespaceBinder : Binder
{
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, ImmutableArray<IMethodSymbol>> _extensionMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, ImmutableArray<IPropertySymbol>> _extensionPropertyCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, ImmutableArray<IMethodSymbol>> _extensionStaticMethodCache = new();
    private readonly ConcurrentDictionary<BinderExtensionLookupKey, ImmutableArray<IPropertySymbol>> _extensionStaticPropertyCache = new();
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
            key => LookupExtensionMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches));

        foreach (var method in methods)
            yield return method;
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var properties = _extensionPropertyCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => LookupExtensionPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches));

        foreach (var property in properties)
            yield return property;
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionStaticMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var methods = _extensionStaticMethodCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => LookupExtensionStaticMethodsCore(key.Name, key.ReceiverType, key.IncludePartialMatches));

        foreach (var method in methods)
            yield return method;
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionStaticProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        var properties = _extensionStaticPropertyCache.GetOrAdd(
            new BinderExtensionLookupKey(name, receiverType, includePartialMatches),
            key => LookupExtensionStaticPropertiesCore(key.Name, key.ReceiverType, key.IncludePartialMatches));

        foreach (var property in properties)
            yield return property;
    }

    private ImmutableArray<IMethodSymbol> LookupExtensionMethodsCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in GetExtensionMethodsFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(method))
                builder.Add(method);
        }

        foreach (var declaredType in _declaredTypes)
        {
            foreach (var method in GetExtensionMethodsFromScope(declaredType, name, receiverType, includePartialMatches))
            {
                if (seen.Add(method))
                    builder.Add(method);
            }
        }

        foreach (var method in base.LookupExtensionMethods(name, receiverType, includePartialMatches))
        {
            if (seen.Add(method))
                builder.Add(method);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IPropertySymbol> LookupExtensionPropertiesCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<IPropertySymbol>(SymbolEqualityComparer.Default);
        var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

        foreach (var property in GetExtensionPropertiesFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(property))
                builder.Add(property);
        }

        foreach (var declaredType in _declaredTypes)
        {
            foreach (var property in GetExtensionPropertiesFromScope(declaredType, name, receiverType, includePartialMatches))
            {
                if (seen.Add(property))
                    builder.Add(property);
            }
        }

        foreach (var property in base.LookupExtensionProperties(name, receiverType, includePartialMatches))
        {
            if (seen.Add(property))
                builder.Add(property);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IMethodSymbol> LookupExtensionStaticMethodsCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
        var builder = ImmutableArray.CreateBuilder<IMethodSymbol>();

        foreach (var method in GetExtensionStaticMethodsFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(method))
                builder.Add(method);
        }

        foreach (var declaredType in _declaredTypes)
        {
            foreach (var method in GetExtensionStaticMethodsFromScope(declaredType, name, receiverType, includePartialMatches))
            {
                if (seen.Add(method))
                    builder.Add(method);
            }
        }

        foreach (var method in base.LookupExtensionStaticMethods(name, receiverType, includePartialMatches))
        {
            if (seen.Add(method))
                builder.Add(method);
        }

        return builder.ToImmutable();
    }

    private ImmutableArray<IPropertySymbol> LookupExtensionStaticPropertiesCore(string? name, ITypeSymbol receiverType, bool includePartialMatches)
    {
        var seen = new HashSet<IPropertySymbol>(SymbolEqualityComparer.Default);
        var builder = ImmutableArray.CreateBuilder<IPropertySymbol>();

        foreach (var property in GetExtensionStaticPropertiesFromScope(_namespaceSymbol, name, receiverType, includePartialMatches))
        {
            if (seen.Add(property))
                builder.Add(property);
        }

        foreach (var declaredType in _declaredTypes)
        {
            foreach (var property in GetExtensionStaticPropertiesFromScope(declaredType, name, receiverType, includePartialMatches))
            {
                if (seen.Add(property))
                    builder.Add(property);
            }
        }

        foreach (var property in base.LookupExtensionStaticProperties(name, receiverType, includePartialMatches))
        {
            if (seen.Add(property))
                builder.Add(property);
        }

        return builder.ToImmutable();
    }
}
