using System.Collections.Concurrent;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class CompilationSymbolLookup
{
    private static readonly object s_missing = new();

    private readonly Compilation _compilation;
    private readonly ConcurrentDictionary<string, object> _namespaceCache = new(StringComparer.Ordinal);
    private readonly ConcurrentDictionary<SimpleNamedTypeLookupKey, object> _simpleNamedTypeCache = new();
    private readonly ConcurrentDictionary<SourceExtensionContainerLookupKey, ImmutableArray<NamedTypeSymbol>> _sourceExtensionContainerCache = new();

    public CompilationSymbolLookup(Compilation compilation)
    {
        _compilation = compilation;
    }

    public NamespaceSymbol? GetNamespace(string? metadataName)
    {
        _compilation.EnsureSetup();

        if (string.IsNullOrEmpty(metadataName))
            return _compilation.GlobalNamespace;

        if (_namespaceCache.TryGetValue(metadataName, out var cached))
            return ReferenceEquals(cached, s_missing) ? null : (NamespaceSymbol)cached;

        var resolved = GetNamespaceUncached(metadataName);
        if (resolved is null && !_compilation.AreSourceDeclarationsDeclared)
            return null;

        _namespaceCache.TryAdd(metadataName, resolved ?? s_missing);
        return resolved;
    }

    public IEnumerable<NamespaceSymbol> EnumerateGlobalNamespaceRoots()
    {
        if (_compilation.SourceGlobalNamespace is not null)
            yield return _compilation.SourceGlobalNamespace;

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
            yield return assembly.GlobalNamespace;
    }

    public ImmutableArray<Symbol> GetGlobalMembers(string name)
    {
        if (string.IsNullOrEmpty(name))
            return ImmutableArray<Symbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<Symbol>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var root in EnumerateGlobalNamespaceRoots())
        {
            foreach (var member in root.GetMembers(name))
            {
                var key = member.GetShallowLookupIdentityKey();
                if (seen.Add(key))
                    builder.Add(member);
            }
        }

        return builder.ToImmutable();
    }

    public ImmutableArray<Symbol> GetGlobalMembersSourceFirst(string name)
    {
        if (string.IsNullOrEmpty(name))
            return ImmutableArray<Symbol>.Empty;

        var sourceMembers = _compilation.SourceGlobalNamespace?.GetMembers(name) ?? ImmutableArray<Symbol>.Empty;
        if (!sourceMembers.IsDefaultOrEmpty)
            return sourceMembers;

        var builder = ImmutableArray.CreateBuilder<Symbol>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
        {
            foreach (var member in assembly.GlobalNamespace.GetMembers(name))
            {
                var key = member.GetShallowLookupIdentityKey();
                if (seen.Add(key))
                    builder.Add(member);
            }
        }

        return builder.ToImmutable();
    }

    public ImmutableArray<Symbol> GetGlobalMembers()
    {
        var builder = ImmutableArray.CreateBuilder<Symbol>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var root in EnumerateGlobalNamespaceRoots())
        {
            foreach (var member in root.GetMembers())
            {
                var key = member.GetShallowLookupIdentityKey();
                if (seen.Add(key))
                    builder.Add(member);
            }
        }

        return builder.ToImmutable();
    }

    public IEnumerable<NamedTypeSymbol> GetAllGlobalTypes()
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        foreach (var root in EnumerateGlobalNamespaceRoots())
        {
            foreach (var type in root.GetAllTypesRecursive())
            {
                if (seen.Add(type.GetShallowLookupIdentityKey()))
                    yield return type;
            }
        }
    }

    public IEnumerable<NamedTypeSymbol> GetExtensionConversionContainers()
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);

        if (_compilation.SourceGlobalNamespace is { } sourceGlobalNamespace)
        {
            foreach (var type in sourceGlobalNamespace.GetAllTypesRecursive())
            {
                if (type.HasStaticExtensionMembers &&
                    seen.Add(type.GetShallowLookupIdentityKey()))
                {
                    yield return type;
                }
            }
        }

        foreach (var assembly in _compilation.ReferencedAssemblySymbols.OfType<ImportedAssemblySymbol>())
        {
            foreach (var type in assembly.GetExtensionConversionContainers())
            {
                if (seen.Add(type.GetShallowLookupIdentityKey()))
                    yield return type;
            }
        }
    }

    public NamespaceSymbol? LookupNamespaceSourceFirst(NamespaceSymbol? currentNamespace, string name, bool includeGlobalFallback = true)
    {
        if (string.IsNullOrEmpty(name))
            return null;

        if (_compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed || !includeGlobalFallback)
        {
            _compilation.DeclareIndexedSourceNamespace(currentNamespace, name);
            if (includeGlobalFallback)
                _compilation.DeclareIndexedSourceNamespace(null, name);
        }

        var candidates = ImmutableArray.CreateBuilder<NamespaceSymbol>();

        var sourceCurrent = currentNamespace is SourceNamespaceSymbol sourceNamespace
            ? sourceNamespace.LookupNamespace(name)
            : null;
        if (sourceCurrent is not null)
            candidates.Add(sourceCurrent);

        if (currentNamespace is not null)
        {
            if (currentNamespace is SourceNamespaceSymbol)
            {
                foreach (var metadataNamespace in GetMetadataNamespacesFor(currentNamespace))
                {
                    if (metadataNamespace.LookupNamespace(name) is { } child)
                        candidates.Add(child);
                }
            }
            else if (currentNamespace.LookupNamespace(name) is { } metadataCurrent)
            {
                candidates.Add(metadataCurrent);
            }
        }

        if (MergeNamespaceCandidates(candidates) is { } scopedNamespace)
            return scopedNamespace;
        if (!includeGlobalFallback)
            return null;

        var sourceGlobal = _compilation.SourceGlobalNamespace?.LookupNamespace(name);
        if (sourceGlobal is not null)
            candidates.Add(sourceGlobal);

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
        {
            if (assembly.GlobalNamespace.LookupNamespace(name) is { } metadataNamespace)
                candidates.Add(metadataNamespace);
        }

        return MergeNamespaceCandidates(candidates);
    }

    public TypeSymbol? LookupTypeSourceFirst(NamespaceSymbol? currentNamespace, string name, bool includeGlobalFallback = true)
    {
        if (string.IsNullOrEmpty(name))
            return null;

        if ((_compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed || !includeGlobalFallback) &&
            _compilation.TryDeclareIndexedSourceType(currentNamespace, name, out var indexedSourceType))
        {
            return indexedSourceType;
        }

        var sourceType = currentNamespace is SourceNamespaceSymbol sourceNamespace
            ? sourceNamespace.LookupTypeDeclared(name)
            : null;
        if (sourceType is not null)
            return sourceType;

        if (currentNamespace is SourceNamespaceSymbol)
        {
            foreach (var metadataNamespace in GetMetadataNamespacesFor(currentNamespace))
                if (metadataNamespace.LookupType(name) is { } siblingType)
                    return CanonicalMetadataType(siblingType);
        }

        if (currentNamespace is not SourceNamespaceSymbol and not null &&
            currentNamespace.LookupType(name) is { } metadataOrMergedType)
        {
            return CanonicalMetadataType(metadataOrMergedType);
        }

        if (!includeGlobalFallback)
            return null;

        sourceType = _compilation.SourceGlobalNamespace?.LookupTypeDeclared(name);
        if (sourceType is not null)
            return sourceType;

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
        {
            if (assembly.GlobalNamespace.LookupType(name) is { } metadataType)
                return CanonicalMetadataType(metadataType);
        }

        return null;

        // Namespace enumeration can expose duplicate bridge/facade declarations in
        // a different order than metadata-name lookup. Use the same canonical
        // selection as ImportBinder, while preserving explicitly declared source types.
        TypeSymbol CanonicalMetadataType(TypeSymbol type) => type is NamedTypeSymbol named
            ? _compilation.GetTypeByMetadataName(named.ToFullyQualifiedMetadataName()) ?? type
            : type;
    }

    public NamedTypeSymbol? GetTypeByMetadataNameSourceFirst(string metadataName)
    {
        if (string.IsNullOrEmpty(metadataName))
            return null;

        return _compilation.GetTypeByMetadataName(metadataName);
    }

    public NamedTypeSymbol? GetTypeByMetadataNameMetadataOnly(string metadataName)
    {
        if (string.IsNullOrEmpty(metadataName))
            return null;

        return _compilation.TryGetMetadataReferenceTypeByMetadataName(metadataName);
    }

    public NamedTypeSymbol? GetTypeBySimpleName(string name, int arity)
    {
        if (string.IsNullOrEmpty(name))
            return null;

        var key = new SimpleNamedTypeLookupKey(name, arity);
        if (_simpleNamedTypeCache.TryGetValue(key, out var cached))
            return ReferenceEquals(cached, s_missing) ? null : (NamedTypeSymbol)cached;

        var resolved = _compilation.SourceGlobalNamespace?
            .GetAllTypesRecursive()
            .FirstOrDefault(candidate =>
                string.Equals(candidate.Name, name, StringComparison.Ordinal) &&
                (candidate.Arity == arity || candidate.TypeParameters.Length == arity));
        resolved ??= _compilation.ReferencedAssemblySymbols
            .OfType<ImportedAssemblySymbol>()
            .Select(assembly => assembly.GetTypeBySimpleName(name, arity))
            .FirstOrDefault(static candidate => candidate is not null);

        if (resolved is null && !_compilation.AreSourceDeclarationsDeclared)
            return null;

        _simpleNamedTypeCache.TryAdd(key, resolved ?? s_missing);
        return resolved;
    }

    public ImmutableArray<NamedTypeSymbol> GetExtensionContainers(
        NamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds)
    {
        if (namespaceSymbol is null || kinds == ExtensionMemberKinds.None)
            return ImmutableArray<NamedTypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<string>(StringComparer.Ordinal);

        if (!_compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed ||
            _compilation.SourceDeclarationsDeclared ||
            namespaceSymbol is not SourceNamespaceSymbol)
        {
            foreach (var container in GetSourceExtensionContainers(namespaceSymbol, memberName, kinds))
            {
                if (seen.Add(container.GetShallowLookupIdentityKey()))
                    builder.Add(container);
            }
        }

        if (!string.IsNullOrEmpty(memberName) &&
            kinds.HasFlag(ExtensionMemberKinds.InstanceMethods))
        {
            foreach (var metadataNamespace in GetMetadataNamespacesFor(namespaceSymbol).OfType<INamespaceExtensionLookup>())
            {
                foreach (var container in metadataNamespace.GetExtensionMethodContainers(memberName!))
                {
                    if (seen.Add(container.GetShallowLookupIdentityKey()))
                        builder.Add(container);
                }
            }
        }

        return builder.ToImmutable();
    }

    private IEnumerable<NamespaceSymbol> GetMetadataNamespacesFor(NamespaceSymbol namespaceSymbol)
    {
        if (namespaceSymbol is INamespaceExtensionLookup)
        {
            yield return namespaceSymbol;
            yield break;
        }

        var metadataName = namespaceSymbol.MetadataName;
        if (string.IsNullOrEmpty(metadataName))
        {
            foreach (var assembly in _compilation.ReferencedAssemblySymbols)
                yield return assembly.GlobalNamespace;

            yield break;
        }

        var parts = metadataName.Split('.', StringSplitOptions.RemoveEmptyEntries);
        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
        {
            if (TryResolve(assembly.GlobalNamespace, parts) is { } metadataNamespace)
                yield return metadataNamespace;
        }
    }

    private readonly record struct SimpleNamedTypeLookupKey(string Name, int Arity);

    private NamespaceSymbol? GetNamespaceUncached(string metadataName)
    {
        var namespaceParts = metadataName.Split('.', StringSplitOptions.RemoveEmptyEntries);
        if (namespaceParts.Length == 0)
            return _compilation.GlobalNamespace;

        var namespaces = ImmutableArray.CreateBuilder<NamespaceSymbol>();

        var source = TryResolve(_compilation.SourceGlobalNamespace, namespaceParts);
        if (source is not null)
            namespaces.Add(source);

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
        {
            var candidate = TryResolve(assembly.GlobalNamespace, namespaceParts);
            if (candidate is not null)
                namespaces.Add(candidate);
        }

        return MergeNamespaceCandidates(namespaces);
    }

    private static NamespaceSymbol? MergeNamespaceCandidates(IEnumerable<NamespaceSymbol> namespaces)
    {
        var unique = namespaces.Distinct().ToImmutableArray();

        return unique.Length switch
        {
            0 => null,
            1 => unique[0],
            _ => new MergedNamespaceSymbol(unique, null!)
        };
    }

    private static NamespaceSymbol? TryResolve(NamespaceSymbol? root, string[] parts)
    {
        if (root is null)
            return null;

        var current = root;
        foreach (var part in parts)
        {
            current = current.LookupNamespace(part)
                ?? current.GetMembers(part)
                    .OfType<NamespaceSymbol>()
                    .FirstOrDefault();

            if (current is null)
                return null;
        }

        return current;
    }

    private ImmutableArray<NamedTypeSymbol> GetSourceExtensionContainers(
        NamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds)
    {
        if (!_compilation.SourceDeclarationsDeclared)
            return GetSourceExtensionContainersUncached(namespaceSymbol, memberName, kinds);

        var key = new SourceExtensionContainerLookupKey(namespaceSymbol.GetShallowLookupIdentityKey(), memberName, kinds);
        return _sourceExtensionContainerCache.GetOrAdd(
            key,
            _ => GetSourceExtensionContainersUncached(namespaceSymbol, memberName, kinds));
    }

    private static ImmutableArray<NamedTypeSymbol> GetSourceExtensionContainersUncached(
        NamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds)
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        CollectSourceExtensionContainers(namespaceSymbol, memberName, kinds, builder);
        return builder.ToImmutable();
    }

    private static void CollectSourceExtensionContainers(
        NamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds,
        ImmutableArray<NamedTypeSymbol>.Builder builder)
    {
        foreach (var member in namespaceSymbol.GetMembers())
        {
            switch (member)
            {
                case SourceNamedTypeSymbol sourceType
                    when SourceExtensionContainerHasRequestedMember(sourceType, memberName, kinds):
                    builder.Add(sourceType);
                    CollectSourceExtensionContainers(sourceType, memberName, kinds, builder);
                    break;

                case SourceNamedTypeSymbol sourceType:
                    CollectSourceExtensionContainers(sourceType, memberName, kinds, builder);
                    break;

                    // Extension scopes are namespace-specific. A namespace import makes
                    // extension containers declared in that namespace available, but it
                    // does not import extension containers from nested namespaces.
            }
        }
    }

    private static void CollectSourceExtensionContainers(
        SourceNamedTypeSymbol sourceType,
        string? memberName,
        ExtensionMemberKinds kinds,
        ImmutableArray<NamedTypeSymbol>.Builder builder)
    {
        var members = string.IsNullOrEmpty(memberName)
            ? sourceType.GetDeclaredMembersWithoutEnsuring()
            : sourceType.GetDeclaredMembersWithoutEnsuring(memberName!);

        foreach (var nested in members.OfType<SourceNamedTypeSymbol>())
        {
            if (SourceExtensionContainerHasRequestedMember(nested, memberName, kinds))
                builder.Add(nested);

            CollectSourceExtensionContainers(nested, memberName, kinds, builder);
        }
    }

    private static bool SourceExtensionContainerHasRequestedMember(
        SourceNamedTypeSymbol sourceType,
        string? memberName,
        ExtensionMemberKinds kinds)
    {
        var members = string.IsNullOrEmpty(memberName)
            ? sourceType.GetDeclaredMembersWithoutEnsuring()
            : sourceType.GetDeclaredMembersWithoutEnsuring(memberName!);

        foreach (var member in members)
        {
            if (member is MethodSymbol method)
            {
                if (kinds.HasFlag(ExtensionMemberKinds.InstanceMethods) && method.IsInstanceExtensionMember)
                    return true;

                if (kinds.HasFlag(ExtensionMemberKinds.StaticMethods) && method.IsStaticExtensionMember)
                    return true;
            }

            if (member is PropertySymbol property)
            {
                if (kinds.HasFlag(ExtensionMemberKinds.InstanceProperties) && property.IsExtensionProperty)
                    return true;

                if (kinds.HasFlag(ExtensionMemberKinds.StaticProperties) && property.IsStaticExtensionMember)
                    return true;
            }
        }

        return false;
    }

    private readonly record struct SourceExtensionContainerLookupKey(
        string NamespaceIdentity,
        string? MemberName,
        ExtensionMemberKinds Kinds);
}
