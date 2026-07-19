using System.Collections.Concurrent;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class CompilationSymbolLookup
{
    private static readonly object s_missing = new();

    private readonly Compilation _compilation;
    private readonly ConcurrentDictionary<string, object> _namespaceCache = new(StringComparer.Ordinal);
    private readonly ConcurrentDictionary<SourceExtensionContainerLookupKey, ImmutableArray<INamedTypeSymbol>> _sourceExtensionContainerCache = new();

    public CompilationSymbolLookup(Compilation compilation)
    {
        _compilation = compilation;
    }

    public INamespaceSymbol? GetNamespace(string? metadataName)
    {
        _compilation.EnsureSetup();

        if (string.IsNullOrWhiteSpace(metadataName))
            return _compilation.GlobalNamespace;

        if (_namespaceCache.TryGetValue(metadataName, out var cached))
            return ReferenceEquals(cached, s_missing) ? null : (INamespaceSymbol)cached;

        var resolved = GetNamespaceUncached(metadataName);
        if (resolved is null && !_compilation.AreSourceDeclarationsDeclared)
            return null;

        _namespaceCache.TryAdd(metadataName, resolved ?? s_missing);
        return resolved;
    }

    public IEnumerable<INamespaceSymbol> EnumerateGlobalNamespaceRoots()
    {
        if (_compilation.SourceGlobalNamespace is not null)
            yield return _compilation.SourceGlobalNamespace;

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
            yield return assembly.GlobalNamespace;
    }

    public ImmutableArray<ISymbol> GetGlobalMembers(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return ImmutableArray<ISymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<ISymbol>();
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

    public ImmutableArray<ISymbol> GetGlobalMembersSourceFirst(string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return ImmutableArray<ISymbol>.Empty;

        var sourceMembers = _compilation.SourceGlobalNamespace?.GetMembers(name) ?? ImmutableArray<ISymbol>.Empty;
        if (!sourceMembers.IsDefaultOrEmpty)
            return sourceMembers;

        var builder = ImmutableArray.CreateBuilder<ISymbol>();
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

    public ImmutableArray<ISymbol> GetGlobalMembers()
    {
        var builder = ImmutableArray.CreateBuilder<ISymbol>();
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

    public IEnumerable<INamedTypeSymbol> GetAllGlobalTypes()
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

    public INamespaceSymbol? LookupNamespaceSourceFirst(INamespaceSymbol? currentNamespace, string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return null;

        var candidates = ImmutableArray.CreateBuilder<INamespaceSymbol>();

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

    public ITypeSymbol? LookupTypeSourceFirst(INamespaceSymbol? currentNamespace, string name)
    {
        if (string.IsNullOrWhiteSpace(name))
            return null;

        if (_compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed &&
            _compilation.TryDeclareIndexedSourceType(currentNamespace, name, out var indexedSourceType))
        {
            return indexedSourceType;
        }

        var sourceType = currentNamespace is SourceNamespaceSymbol sourceNamespace
            ? sourceNamespace.LookupTypeDeclared(name)
            : null;
        if (sourceType is not null)
            return sourceType;

        if (currentNamespace is not SourceNamespaceSymbol and not null &&
            currentNamespace.LookupType(name) is { } metadataOrMergedType)
        {
            return metadataOrMergedType;
        }

        sourceType = _compilation.SourceGlobalNamespace?.LookupTypeDeclared(name);
        if (sourceType is not null)
            return sourceType;

        foreach (var assembly in _compilation.ReferencedAssemblySymbols)
        {
            if (assembly.GlobalNamespace.LookupType(name) is { } metadataType)
                return metadataType;
        }

        return null;
    }

    public INamedTypeSymbol? GetTypeByMetadataNameSourceFirst(string metadataName)
    {
        if (string.IsNullOrWhiteSpace(metadataName))
            return null;

        return _compilation.GetTypeByMetadataName(metadataName);
    }

    public INamedTypeSymbol? GetTypeByMetadataNameMetadataOnly(string metadataName)
    {
        if (string.IsNullOrWhiteSpace(metadataName))
            return null;

        return _compilation.TryGetMetadataReferenceTypeByMetadataName(metadataName);
    }

    public ImmutableArray<INamedTypeSymbol> GetExtensionContainers(
        INamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds)
    {
        if (namespaceSymbol is null || kinds == ExtensionMemberKinds.None)
            return ImmutableArray<INamedTypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
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

        if (!string.IsNullOrWhiteSpace(memberName) &&
            kinds.HasFlag(ExtensionMemberKinds.InstanceMethods))
        {
            foreach (var metadataNamespace in GetMetadataNamespacesFor(namespaceSymbol))
            {
                foreach (var container in GetMetadataExtensionMethodContainers(metadataNamespace, memberName!))
                {
                    if (seen.Add(container.GetShallowLookupIdentityKey()))
                        builder.Add(container);
                }
            }
        }

        return builder.ToImmutable();
    }

    private IEnumerable<INamespaceSymbol> GetMetadataNamespacesFor(INamespaceSymbol namespaceSymbol)
    {
        switch (namespaceSymbol)
        {
            case PENamespaceSymbol:
            case MergedNamespaceSymbol:
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

    private INamespaceSymbol? GetNamespaceUncached(string metadataName)
    {
        var namespaceParts = metadataName.Split('.', StringSplitOptions.RemoveEmptyEntries);
        if (namespaceParts.Length == 0)
            return _compilation.GlobalNamespace;

        var namespaces = ImmutableArray.CreateBuilder<INamespaceSymbol>();

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

    private static INamespaceSymbol? MergeNamespaceCandidates(IEnumerable<INamespaceSymbol> namespaces)
    {
        var unique = namespaces.Distinct().ToImmutableArray();

        return unique.Length switch
        {
            0 => null,
            1 => unique[0],
            _ => new MergedNamespaceSymbol(unique, null!)
        };
    }

    private static INamespaceSymbol? TryResolve(INamespaceSymbol? root, string[] parts)
    {
        if (root is null)
            return null;

        var current = root;
        foreach (var part in parts)
        {
            current = current.LookupNamespace(part)
                ?? current.GetMembers(part)
                    .OfType<INamespaceSymbol>()
                    .FirstOrDefault();

            if (current is null)
                return null;
        }

        return current;
    }

    private ImmutableArray<INamedTypeSymbol> GetSourceExtensionContainers(
        INamespaceSymbol namespaceSymbol,
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

    private static ImmutableArray<INamedTypeSymbol> GetSourceExtensionContainersUncached(
        INamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds)
    {
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        CollectSourceExtensionContainers(namespaceSymbol, memberName, kinds, builder);
        return builder.ToImmutable();
    }

    private static void CollectSourceExtensionContainers(
        INamespaceSymbol namespaceSymbol,
        string? memberName,
        ExtensionMemberKinds kinds,
        ImmutableArray<INamedTypeSymbol>.Builder builder)
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
        ImmutableArray<INamedTypeSymbol>.Builder builder)
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
            if (member is IMethodSymbol method)
            {
                if (kinds.HasFlag(ExtensionMemberKinds.InstanceMethods) && method.IsInstanceExtensionMember)
                    return true;

                if (kinds.HasFlag(ExtensionMemberKinds.StaticMethods) && method.IsStaticExtensionMember)
                    return true;
            }

            if (member is IPropertySymbol property)
            {
                if (kinds.HasFlag(ExtensionMemberKinds.InstanceProperties) && property.IsExtensionProperty)
                    return true;

                if (kinds.HasFlag(ExtensionMemberKinds.StaticProperties) && property.IsStaticExtensionMember)
                    return true;
            }
        }

        return false;
    }

    private static ImmutableArray<INamedTypeSymbol> GetMetadataExtensionMethodContainers(
        INamespaceSymbol namespaceSymbol,
        string methodName)
    {
        return namespaceSymbol switch
        {
            PENamespaceSymbol peNamespace => peNamespace.GetExtensionMethodContainers(methodName),
            MergedNamespaceSymbol mergedNamespace => mergedNamespace.GetExtensionMethodContainers(methodName),
            _ => ImmutableArray<INamedTypeSymbol>.Empty
        };
    }

    private readonly record struct SourceExtensionContainerLookupKey(
        string NamespaceIdentity,
        string? MemberName,
        ExtensionMemberKinds Kinds);
}
