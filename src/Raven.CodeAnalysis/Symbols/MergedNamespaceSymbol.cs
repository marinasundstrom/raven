using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class MergedNamespaceSymbol : Symbol, INamespaceSymbol
{
    private readonly SourceNamespaceSymbol _source;
    private readonly MetadataNamespaceSymbol _metadata;

    public MergedNamespaceSymbol(SourceNamespaceSymbol source, MetadataNamespaceSymbol metadata)
        : base(SymbolKind.Namespace,
               source.Name,
               source.ContainingSymbol,
               null,
               source.ContainingNamespace ?? metadata.ContainingNamespace,
               [],
               [])
    {
        _source = source;
        _metadata = metadata;
    }

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public ImmutableArray<ISymbol> GetMembers()
    {
        var combined = new List<ISymbol>(_source.GetMembers());
        combined.AddRange(_metadata.GetMembers());
        return combined.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        var combined = new List<ISymbol>(_source.GetMembers(name));
        combined.AddRange(_metadata.GetMembers(name));
        return combined.ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        if (_source.IsMemberDefined(name, out symbol)) return true;
        if (_metadata.IsMemberDefined(name, out symbol)) return true;

        symbol = null;
        return false;
    }

    public INamespaceSymbol? LookupNamespace(string name)
    {
        var sourceChild = _source.LookupNamespace(name) as SourceNamespaceSymbol;
        var metadataChild = _metadata.LookupNamespace(name) as MetadataNamespaceSymbol;

        if (sourceChild is not null && metadataChild is not null)
            return new MergedNamespaceSymbol(sourceChild, metadataChild);

        return sourceChild ?? (INamespaceSymbol?)metadataChild;
    }

    public ITypeSymbol? LookupType(string name)
    {
        // You can prioritize source over metadata, or merge lists if needed
        return _source.LookupType(name) ?? _metadata.LookupType(name);
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