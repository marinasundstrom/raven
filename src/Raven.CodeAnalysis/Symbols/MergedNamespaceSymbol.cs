using System.Collections.Immutable;
using System.Diagnostics;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class MergedNamespaceSymbol : Symbol, INamespaceSymbol
{
    private readonly ImmutableArray<INamespaceSymbol> _namespaces;

    public MergedNamespaceSymbol(IEnumerable<INamespaceSymbol> namespaces, INamespaceSymbol containingNamespace)
        : base(SymbolKind.Namespace,
               namespaces.First().Name,
               containingNamespace,
               null,
               containingNamespace,
               [], [])
    {
        _namespaces = Flatten(namespaces).ToImmutableArray();
    }

    public override SymbolKind Kind => SymbolKind.Namespace;

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public ImmutableArray<ISymbol> GetMembers()
    {
        var namespaceGroups = new Dictionary<string, List<INamespaceSymbol>>();
        var otherSymbols = new List<ISymbol>();
        var seen = new HashSet<ISymbol>();

        foreach (var ns in _namespaces)
        {
            foreach (var member in ns.GetMembers())
            {
                if (member is INamespaceSymbol nsMember)
                {
                    if (!namespaceGroups.TryGetValue(nsMember.Name, out var group))
                    {
                        group = new List<INamespaceSymbol>();
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
                otherSymbols.Add(new MergedNamespaceSymbol(unique.OfType<INamespaceSymbol>(), this));
        }

        return otherSymbols.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        var matches = new List<INamespaceSymbol>();
        var others = new List<ISymbol>();
        var seen = new HashSet<ISymbol>();

        foreach (var ns in _namespaces)
        {
            foreach (var member in ns.GetMembers(name))
            {
                if (member is INamespaceSymbol nsMember)
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
            others.Add(new MergedNamespaceSymbol(unique.OfType<INamespaceSymbol>(), this));

        return others.ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        foreach (var ns in _namespaces)
        {
            if (ns.IsMemberDefined(name, out symbol))
                return true;
        }

        symbol = null;
        return false;
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        var found = new List<INamespaceSymbol>();

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
            _ => new MergedNamespaceSymbol(found, null)
        };
    }

    public ITypeSymbol? LookupType(string name)
    {
        foreach (var ns in _namespaces)
        {
            var type = ns.LookupType(name);
            if (type is not null)
                return type;
        }

        return null;
    }

    public string ToMetadataName()
    {
        var parts = new Stack<string>();
        INamespaceSymbol current = this;

        while (!current.IsGlobalNamespace)
        {
            parts.Push(current.Name);
            current = current.ContainingNamespace!;
        }

        return string.Join(".", parts);
    }

    public override string ToString() => IsGlobalNamespace ? "<global>" : this.ToDisplayString();

    private static IEnumerable<INamespaceSymbol> Flatten(IEnumerable<INamespaceSymbol> namespaces)
    {
        foreach (var ns in namespaces)
        {
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

    public IEnumerable<INamespaceSymbol> GetMergedNamespaces() => _namespaces;
}