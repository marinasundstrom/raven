using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class MergedNamespaceSymbol : Symbol, INamespaceSymbol
{
    private readonly ImmutableArray<INamespaceSymbol> _namespaces;

    public MergedNamespaceSymbol(params IEnumerable<INamespaceSymbol> namespaces)
        : base(SymbolKind.Namespace,
               namespaces.First().Name,
               namespaces.First().ContainingSymbol,
               null,
               namespaces.First().ContainingNamespace,
               [], [])
    {
        _namespaces = namespaces.ToImmutableArray();
    }

    public override SymbolKind Kind => SymbolKind.Namespace;

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public ImmutableArray<ISymbol> GetMembers()
    {
        var namespaceGroups = new Dictionary<string, List<INamespaceSymbol>>();
        var otherSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

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

                    group.Add(nsMember);
                }
                else
                {
                    otherSymbols.Add(member);
                }
            }
        }

        // Merge namespace groups
        foreach (var group in namespaceGroups.Values)
        {
            if (group.Count == 1)
                otherSymbols.Add(group[0]);
            else
                otherSymbols.Add(new MergedNamespaceSymbol(group));
        }

        return otherSymbols.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        var matches = new List<INamespaceSymbol>();
        var others = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        foreach (var ns in _namespaces)
        {
            foreach (var member in ns.GetMembers(name))
            {
                if (member is INamespaceSymbol nsMember)
                    matches.Add(nsMember);
                else
                    others.Add(member);
            }
        }

        if (matches.Count > 0)
        {
            if (matches.Count == 1)
                others.Add(matches[0]);
            else
                others.Add(new MergedNamespaceSymbol(matches));
        }

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
                found.Add(child);
        }

        return found.Count switch
        {
            0 => null,
            1 => found[0],
            _ => new MergedNamespaceSymbol(found)
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
}

/*
internal sealed partial class MergedNamespaceSymbol : Symbol, INamespaceSymbol
{
    private readonly INamespaceSymbol _left;
    private readonly INamespaceSymbol _right;

    public MergedNamespaceSymbol(INamespaceSymbol left, INamespaceSymbol right)
        : base(SymbolKind.Namespace,
               left.Name,
               left.ContainingSymbol,
               null,
               left.ContainingNamespace ?? right.ContainingNamespace,
               [],
               [])
    {
        _left = left;
        _right = right;
    }

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public ImmutableArray<ISymbol> GetMembers()
    {
        var combined = new List<ISymbol>(_left.GetMembers());
        combined.AddRange(_right.GetMembers());
        return combined.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        var combined = new List<ISymbol>(_left.GetMembers(name));
        combined.AddRange(_right.GetMembers(name));
        return combined.ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        if (_left.IsMemberDefined(name, out symbol)) return true;
        if (_right.IsMemberDefined(name, out symbol)) return true;

        symbol = null;
        return false;
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        var sourceChild = _left.LookupNamespace(name);
        var metadataChild = _right.LookupNamespace(name);

        if (sourceChild is not null && metadataChild is not null)
            return new MergedNamespaceSymbol(sourceChild, metadataChild);

        return sourceChild ?? metadataChild;
    }

    public ITypeSymbol? LookupType(string name)
    {
        // You can prioritize source over metadata, or merge lists if needed
        return _left.LookupType(name) ?? _right.LookupType(name);
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
}
*/