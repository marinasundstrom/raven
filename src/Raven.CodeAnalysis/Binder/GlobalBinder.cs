using System.Collections.Generic;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

sealed class GlobalBinder : Binder
{
    private readonly INamespaceSymbol _currentNamespace;

    public GlobalBinder(Compilation compilation) : base(null!)
    {
        Compilation = compilation;
        compilation.EnsureSetup();
        _currentNamespace = compilation.SourceGlobalNamespace;
    }

    public override Compilation Compilation { get; }

    public override INamespaceSymbol? CurrentNamespace => _currentNamespace;

    public override ITypeSymbol? LookupType(string name)
    {
        var type = base.LookupType(name); // Look in CurrentNamespace

        if (type != null)
            return type;

        // Fallback to global metadata-based lookup. Semantic-query binder paths
        // suppress source namespace completion and should not initialize all
        // source declarations just to probe a metadata type.
        return Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed
            ? Compilation.SymbolLookup.GetTypeByMetadataNameMetadataOnly(name)
            : Compilation.SymbolLookup.GetTypeByMetadataNameSourceFirst(name);
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        foreach (var method in GetExtensionMethodsFromScope(_currentNamespace, name, receiverType, includePartialMatches))
            yield return method;
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionMethodsByName(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            yield break;

        foreach (var method in GetExtensionMethodsByNameFromScope(_currentNamespace, name))
            yield return method;
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        foreach (var property in GetExtensionPropertiesFromScope(_currentNamespace, name, receiverType, includePartialMatches))
            yield return property;
    }

    public override IEnumerable<IMethodSymbol> LookupExtensionStaticMethods(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        foreach (var method in GetExtensionStaticMethodsFromScope(_currentNamespace, name, receiverType, includePartialMatches))
            yield return method;
    }

    public override IEnumerable<IPropertySymbol> LookupExtensionStaticProperties(string? name, ITypeSymbol receiverType, bool includePartialMatches = false)
    {
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            yield break;

        foreach (var property in GetExtensionStaticPropertiesFromScope(_currentNamespace, name, receiverType, includePartialMatches))
            yield return property;
    }
}
