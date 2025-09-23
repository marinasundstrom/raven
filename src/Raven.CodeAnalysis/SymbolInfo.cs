using System.Collections.Immutable;
using System.Diagnostics;

namespace Raven.CodeAnalysis;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly struct SymbolInfo
{
    internal SymbolInfo(ISymbol? symbol) : this()
    {
        Symbol = symbol;
        CandidateReason = CandidateReason.None;
        CandidateSymbols = symbol is null ? ImmutableArray<ISymbol>.Empty : ImmutableArray.Create(symbol);
    }

    internal SymbolInfo(ISymbol? symbol, ImmutableArray<ISymbol> candidateSymbols, CandidateReason candidateReason = CandidateReason.None) : this()
    {
        Symbol = symbol;
        CandidateSymbols = candidateSymbols;
        CandidateReason = candidateReason;
    }

    internal SymbolInfo(CandidateReason candidateReason, ImmutableArray<ISymbol> candidateSymbols) : this()
    {
        CandidateReason = candidateReason;
        CandidateSymbols = candidateSymbols;
    }

    public CandidateReason CandidateReason { get; }

    public ImmutableArray<ISymbol> CandidateSymbols { get; }

    public ISymbol? Symbol { get; }

    public static readonly SymbolInfo None = new SymbolInfo(CandidateReason.None, []);

    public bool Success => Symbol is not null;

    public override string ToString()
    {
        return Symbol?.ToString()
            ?? (!CandidateSymbols.IsDefaultOrEmpty
                ? $"Candidate: {CandidateSymbols[0]} (Reason: {CandidateReason})"
                : "None");
    }

    private string GetDebuggerDisplay()
    {
        return ToString();
    }
}
