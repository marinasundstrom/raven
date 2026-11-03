using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceNamespaceSymbol : SourceSymbol, NamespaceSymbol
{
    private readonly List<Symbol> _members = new();
    private readonly object _membersGate = new();
    private readonly SourceModuleSymbol? _containingModule;

    public SourceNamespaceSymbol(SourceModuleSymbol containingModule, string name, Symbol containingSymbol,
        NamedTypeSymbol? containingType, NamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        _containingModule = containingModule;
    }

    public SourceNamespaceSymbol(string name, Symbol containingSymbol,
        NamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
    : base(SymbolKind.Namespace, name, containingSymbol, null, containingNamespace, locations, declaringSyntaxReferences)
    {

    }

    public override AssemblySymbol ContainingAssembly => ContainingModule!.ContainingAssembly!;

    public override ModuleSymbol ContainingModule => _containingModule ?? ContainingSymbol!.ContainingModule!;

    public bool IsModule => ContainingAssembly is SourceAssemblySymbol { Compilation.Options.TargetPlatform: TargetPlatform.NeoCLR } || DeclaringSyntaxReferences.Any(reference => reference.GetSyntax() switch
    {
        Syntax.NamespaceDeclarationSyntax declaration => declaration.NamespaceKeyword.Kind == Syntax.SyntaxKind.ModuleKeyword,
        Syntax.FileScopedNamespaceDeclarationSyntax declaration => declaration.NamespaceKeyword.Kind == Syntax.SyntaxKind.ModuleKeyword,
        _ => false
    });
    public bool IsNamespace => true;
    public bool IsType => false;
    public bool IsGlobalNamespace => ContainingNamespace is null;

    public override string MetadataName => IsGlobalNamespace ? "" : ToMetadataName();

    private void EnsureSourceDeclarationsDeclared()
    {
        if (ContainingAssembly is not SourceAssemblySymbol sourceAssembly)
            return;

        if (sourceAssembly.Compilation.SourceDeclarationsDeclared)
            return;

        if (!sourceAssembly.Compilation.IsSourceNamespaceLookupDeclarationCompletionSuppressed)
        {
            sourceAssembly.Compilation.EnsureSourceDeclarationsDeclared();
        }
    }

    internal void AddMember(Symbol member)
    {
        lock (_membersGate)
            _members.Add(member);
    }

    public ImmutableArray<Symbol> GetMembers()
    {
        lock (_membersGate)
            return _members.ToImmutableArray();
    }

    public ImmutableArray<Symbol> GetMembers(string name)
    {
        lock (_membersGate)
            return _members.Where(m => m.Name == name).ToImmutableArray();
    }

    public NamespaceSymbol? LookupNamespace(string name)
    {
        lock (_membersGate)
            return _members.OfType<NamespaceSymbol>().FirstOrDefault(ns => ns.Name == name);
    }

    public TypeSymbol? LookupType(string name)
    {
        EnsureSourceDeclarationsDeclared();
        return LookupTypeDeclared(name);
    }

    internal TypeSymbol? LookupTypeDeclared(string name)
    {
        ImmutableArray<TypeSymbol> candidates;
        lock (_membersGate)
            candidates = _members.OfType<TypeSymbol>().Where(t => t.Name == name).ToImmutableArray();

        return TypeLookupUtilities.SelectBestTypeByName(candidates);
    }

    public override string ToString() => IsGlobalNamespace ? "<global>" : ToMetadataName();

    public string ToMetadataName()
    {
        var parts = new Stack<string>();
        NamespaceSymbol current = this;

        while (!current.IsGlobalNamespace)
        {
            parts.Push(current.Name);
            current = current.ContainingNamespace!;
        }

        return string.Join(".", parts);
    }

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        lock (_membersGate)
            symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }
}
