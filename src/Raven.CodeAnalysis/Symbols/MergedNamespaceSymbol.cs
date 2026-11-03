using System.Collections.Immutable;
using System.Diagnostics;

using Raven.CodeAnalysis.Metadata;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class MergedNamespaceSymbol : SymbolBase, NamespaceSymbol, INamespaceExtensionLookup
{
    private readonly ImmutableArray<NamespaceSymbol> _namespaces;

    public MergedNamespaceSymbol(IEnumerable<NamespaceSymbol> namespaces, NamespaceSymbol? containingNamespace)
        : this(CreateState(namespaces, containingNamespace))
    {
    }

    private MergedNamespaceSymbol(MergedNamespaceState state)
        : base(SymbolKind.Namespace,
               state.Namespaces[0].Name,
               state.ContainingNamespace,
               null,
               state.ContainingNamespace,
               [], [])
    {
        _namespaces = state.Namespaces;
    }

    public override Documentation.DocumentationComment? GetDocumentationComment()
    {
        var comments = _namespaces.Select(ns => ns.GetDocumentationComment()).OfType<Documentation.DocumentationComment>().ToArray();
        return comments.Length switch
        {
            0 => null,
            1 => comments[0],
            _ => Documentation.DocumentationComment.Merge(comments[0].Format, comments)
        };
    }

    public override SymbolKind Kind => SymbolKind.Namespace;

    public bool IsModule => _namespaces.Any(ns => ns.IsModule);
    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => _namespaces.All(static namespaceSymbol => namespaceSymbol.IsGlobalNamespace);

    public ImmutableArray<Symbol> GetMembers()
    {
        var namespaceGroups = new Dictionary<string, List<NamespaceSymbol>>();
        var otherSymbols = new List<Symbol>();
        var seen = new HashSet<Symbol>();

        foreach (var ns in _namespaces)
        {
            foreach (var member in ns.GetMembers())
            {
                if (member is NamespaceSymbol nsMember)
                {
                    if (!namespaceGroups.TryGetValue(nsMember.Name, out var group))
                    {
                        group = new List<NamespaceSymbol>();
                        namespaceGroups[nsMember.Name] = group;
                    }

                    if (nsMember is MergedNamespaceSymbol merged)
                        group.AddRange(merged._namespaces);
                    else
                        group.Add(nsMember);
                }
                else
                {
                    if (seen.Add(member))
                        otherSymbols.Add(member);
                }
            }
        }

        foreach (var (name, group) in namespaceGroups)
        {
            var unique = group.Distinct().ToList();
            if (unique.Count == 1)
                otherSymbols.Add(unique[0]);
            else
                otherSymbols.Add(new MergedNamespaceSymbol(unique.OfType<NamespaceSymbol>(), this));
        }

        return otherSymbols.ToImmutableArray();
    }

    public ImmutableArray<Symbol> GetMembers(string name)
    {
        var matches = new List<NamespaceSymbol>();
        var others = new List<Symbol>();
        var seen = new HashSet<Symbol>();

        foreach (var ns in _namespaces)
        {
            foreach (var member in ns.GetMembers(name))
            {
                if (member is NamespaceSymbol nsMember)
                {
                    if (nsMember is MergedNamespaceSymbol merged)
                        matches.AddRange(merged._namespaces);
                    else
                        matches.Add(nsMember);
                }
                else
                {
                    if (seen.Add(member))
                        others.Add(member);
                }
            }
        }

        var unique = matches.Distinct().ToList();
        if (unique.Count == 1)
            others.Add(unique[0]);
        else if (unique.Count > 1)
            others.Add(new MergedNamespaceSymbol(unique.OfType<NamespaceSymbol>(), this));

        return others.ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        foreach (var ns in _namespaces)
        {
            if (ns.IsMemberDefined(name, out symbol))
                return true;
        }

        symbol = null;
        return false;
    }

    public NamespaceSymbol? LookupNamespace(string name)
    {
        var found = new List<NamespaceSymbol>();

        foreach (var ns in _namespaces)
        {
            var child = ns.LookupNamespace(name);
            if (child is not null)
            {
                if (child is MergedNamespaceSymbol merged)
                    found.AddRange(merged._namespaces);
                else
                    found.Add(child);
            }
        }

        return found.Count switch
        {
            0 => null,
            1 => found[0],
            _ => new MergedNamespaceSymbol(found, this)
        };
    }

    public TypeSymbol? LookupType(string name)
    {
        var candidates = ImmutableArray.CreateBuilder<TypeSymbol>();

        foreach (var ns in _namespaces)
        {
            if (ns.LookupType(name) is { } type)
                candidates.Add(type);
        }

        return TypeLookupUtilities.SelectBestTypeByName(candidates.ToImmutable());
    }

    public ImmutableArray<NamedTypeSymbol> GetExtensionMethodContainers(string methodName)
    {
        if (string.IsNullOrWhiteSpace(methodName))
            return ImmutableArray<NamedTypeSymbol>.Empty;

        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<Symbol>(SymbolEqualityComparer.Default);

        foreach (var ns in _namespaces)
        {
            if (ns is not INamespaceExtensionLookup lookup)
                continue;

            foreach (var container in lookup.GetExtensionMethodContainers(methodName))
            {
                if (seen.Add(container))
                    builder.Add(container);
            }
        }

        return builder.ToImmutable();
    }

    public string ToMetadataName()
        => IsGlobalNamespace ? string.Empty : _namespaces[0].ToMetadataName();

    public override string ToString() => IsGlobalNamespace ? "<global>" : ToMetadataName();

    private static IEnumerable<NamespaceSymbol> Flatten(IEnumerable<NamespaceSymbol> namespaces)
    {
        foreach (var ns in namespaces)
        {
            if (ns is null)
                continue;

            if (ns is MergedNamespaceSymbol merged)
            {
                foreach (var inner in merged._namespaces)
                    yield return inner;
            }
            else
            {
                yield return ns;
            }
        }
    }

    private static ImmutableArray<NamespaceSymbol> CreateMergedNamespaces(IEnumerable<NamespaceSymbol> namespaces)
    {
        ArgumentNullException.ThrowIfNull(namespaces);

        var flattened = Flatten(namespaces).ToImmutableArray();

        if (flattened.IsEmpty)
            throw new ArgumentException("At least one namespace must be provided.", nameof(namespaces));

        return flattened;
    }

    private static MergedNamespaceState CreateState(
        IEnumerable<NamespaceSymbol> namespaces,
        NamespaceSymbol? containingNamespace)
    {
        var mergedNamespaces = CreateMergedNamespaces(namespaces);
        if (containingNamespace is not null ||
            mergedNamespaces.All(static namespaceSymbol => namespaceSymbol.IsGlobalNamespace))
        {
            return new MergedNamespaceState(mergedNamespaces, containingNamespace);
        }

        var containingNamespaces = mergedNamespaces
            .Select(static namespaceSymbol => namespaceSymbol.ContainingNamespace)
            .Where(static namespaceSymbol => namespaceSymbol is not null)
            .Cast<NamespaceSymbol>()
            .Distinct()
            .ToImmutableArray();
        containingNamespace = containingNamespaces.Length switch
        {
            0 => null,
            1 => containingNamespaces[0],
            _ => new MergedNamespaceSymbol(containingNamespaces, null)
        };

        return new MergedNamespaceState(mergedNamespaces, containingNamespace);
    }

    private readonly record struct MergedNamespaceState(
        ImmutableArray<NamespaceSymbol> Namespaces,
        NamespaceSymbol? ContainingNamespace);

    public IEnumerable<NamespaceSymbol> GetMergedNamespaces() => _namespaces;
}
