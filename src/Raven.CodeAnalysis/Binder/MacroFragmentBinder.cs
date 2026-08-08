using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed class MacroFragmentBinder : BlockBinder
{
    private readonly ImmutableArray<ISymbol> _fragmentSymbols;

    public MacroFragmentBinder(
        Binder parent,
        ImmutableArray<MacroFragmentLocal> fragmentLocals,
        ImmutableArray<ISymbol> visibleSymbols)
        : base(
            parent.ContainingSymbol ?? parent.Compilation.GlobalNamespace,
            parent)
    {
        var builder = ImmutableArray.CreateBuilder<ISymbol>(fragmentLocals.Length + visibleSymbols.Length);
        foreach (var local in fragmentLocals)
        {
            builder.Add(new SourceLocalSymbol(
                local.Name,
                local.Type,
                isMutable: false,
                ContainingSymbol,
                ContainingSymbol.ContainingType,
                ContainingSymbol as INamespaceSymbol ?? ContainingSymbol.ContainingNamespace,
                locations: [],
                declaringSyntaxReferences: []));
        }

        builder.AddRange(visibleSymbols);
        _fragmentSymbols = builder.ToImmutable();
    }

    public override ISymbol? LookupSymbol(string name)
        => LookupSymbols(name).FirstOrDefault();

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        foreach (var symbol in _fragmentSymbols)
        {
            if (string.Equals(symbol.Name, name, StringComparison.Ordinal) &&
                seen.Add(symbol.GetLookupIdentityKey()))
            {
                yield return symbol;
            }
        }

        foreach (var symbol in base.LookupSymbols(name))
        {
            if (seen.Add(symbol.GetLookupIdentityKey()))
                yield return symbol;
        }
    }

    public override IEnumerable<ISymbol> LookupAvailableSymbols()
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        foreach (var symbol in _fragmentSymbols)
        {
            if (seen.Add(symbol.GetLookupIdentityKey()))
                yield return symbol;
        }

        foreach (var symbol in base.LookupAvailableSymbols())
        {
            if (seen.Add(symbol.GetLookupIdentityKey()))
                yield return symbol;
        }
    }
}
