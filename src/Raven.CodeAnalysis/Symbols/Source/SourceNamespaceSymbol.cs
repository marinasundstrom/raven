using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceNamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    private readonly List<ISymbol> _members = new();
    private readonly SourceModuleSymbol _containingModule;

    public SourceNamespaceSymbol(SourceModuleSymbol containingModule, string name, ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        _containingModule = containingModule;
    }

    public SourceNamespaceSymbol(string name, ISymbol containingSymbol,
        INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
    : base(SymbolKind.Namespace, name, containingSymbol, null, containingNamespace, locations, declaringSyntaxReferences)
    {

    }

    public override IAssemblySymbol ContainingAssembly => ContainingModule!.ContainingAssembly!;

    public override IModuleSymbol ContainingModule => _containingModule ?? ContainingSymbol!.ContainingModule!;

    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public override string MetadataName => IsGlobalNamespace ? "" : ToMetadataName();


    internal void AddMember(ISymbol member) => _members.Add(member);

    public ImmutableArray<ISymbol> GetMembers() => _members.ToImmutableArray();

    public ImmutableArray<ISymbol> GetMembers(string name) =>
        _members.Where(m => m.Name == name).ToImmutableArray();

    public INamespaceSymbol? LookupNamespace(string name) =>
        _members.OfType<INamespaceSymbol>().FirstOrDefault(ns => ns.Name == name);

    public ITypeSymbol? LookupType(string name) =>
        _members.OfType<ITypeSymbol>().FirstOrDefault(t => t.Name == name);

    public override string ToString() => IsGlobalNamespace ? "<global>" : this.ToDisplayString();

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

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }
}