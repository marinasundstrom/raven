using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal readonly record struct MacroFragmentVisibleSymbol(string Name, ISymbol Symbol);

internal sealed class MacroFragmentBinder : BlockBinder
{
    private readonly ImmutableArray<MacroFragmentVisibleSymbol> _fragmentSymbols;
    private readonly Dictionary<(int Start, int Length), ImmutableArray<MacroFragmentVisibleSymbol>> _nestedMacroVisibleSymbols = new();

    public MacroFragmentBinder(
        Binder parent,
        ImmutableArray<MacroFragmentLocal> fragmentLocals,
        ImmutableArray<MacroFragmentVisibleSymbol> visibleSymbols,
        SyntaxTree syntaxTree)
        : base(
            parent.ContainingSymbol ?? parent.Compilation.GlobalNamespace,
            parent)
    {
        var builder = ImmutableArray.CreateBuilder<MacroFragmentVisibleSymbol>(fragmentLocals.Length + visibleSymbols.Length);
        foreach (var local in fragmentLocals)
        {
            var locations = local.DeclarationSpan is { } declarationSpan
                ? new[] { Location.Create(syntaxTree, declarationSpan) }
                : [];
            var symbol = new SourceLocalSymbol(
                local.Name,
                local.Type,
                isMutable: false,
                ContainingSymbol,
                ContainingSymbol.ContainingType,
                ContainingSymbol as INamespaceSymbol ?? ContainingSymbol.ContainingNamespace,
                locations,
                declaringSyntaxReferences: []);
            builder.Add(new MacroFragmentVisibleSymbol(local.Name, symbol));
        }

        builder.AddRange(visibleSymbols);
        _fragmentSymbols = builder.ToImmutable();
    }

    public override ISymbol? LookupSymbol(string name)
        => LookupSymbols(name).FirstOrDefault();

    public override IEnumerable<ISymbol> LookupSymbols(string name)
    {
        var seen = new HashSet<string>(StringComparer.Ordinal);
        foreach (var visibleSymbol in _fragmentSymbols)
        {
            if (string.Equals(visibleSymbol.Name, name, StringComparison.Ordinal) &&
                seen.Add(visibleSymbol.Symbol.GetLookupIdentityKey()))
            {
                yield return visibleSymbol.Symbol;
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
        foreach (var visibleSymbol in _fragmentSymbols)
        {
            if (seen.Add(visibleSymbol.Symbol.GetLookupIdentityKey()))
                yield return visibleSymbol.Symbol;
        }

        foreach (var symbol in base.LookupAvailableSymbols())
        {
            if (seen.Add(symbol.GetLookupIdentityKey()))
                yield return symbol;
        }
    }

    protected override void OnInvocableMacroExpressionBinding(InvocableMacroExpressionSyntax syntax)
    {
        var builder = ImmutableArray.CreateBuilder<MacroFragmentVisibleSymbol>();
        var localSymbols = new HashSet<ISymbol>(ReferenceEqualityComparer.Instance);
        foreach (var (name, local) in _locals)
        {
            builder.Add(new MacroFragmentVisibleSymbol(name, local.Symbol));
            localSymbols.Add(local.Symbol);
        }

        foreach (var symbol in LookupAvailableSymbols())
        {
            if (!localSymbols.Contains(symbol))
                builder.Add(new MacroFragmentVisibleSymbol(symbol.Name, symbol));
        }

        _nestedMacroVisibleSymbols[(syntax.Span.Start, syntax.Span.Length)] = builder.ToImmutable();
    }

    internal bool TryGetNestedMacroVisibleSymbols(
        InvocableMacroExpressionSyntax syntax,
        out ImmutableArray<MacroFragmentVisibleSymbol> visibleSymbols)
        => _nestedMacroVisibleSymbols.TryGetValue((syntax.Span.Start, syntax.Span.Length), out visibleSymbols);

    internal static ImmutableArray<MacroFragmentVisibleSymbol> CreateVisibleSymbols(IEnumerable<ISymbol> symbols)
        => symbols.Select(static symbol => new MacroFragmentVisibleSymbol(symbol.Name, symbol)).ToImmutableArray();
}
