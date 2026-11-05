using System.Collections.Immutable;
using System.Diagnostics;

namespace Raven.CodeAnalysis;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
public readonly record struct SymbolInfo
{
    internal SymbolInfo(Symbol? symbol) : this()
    {
        Symbol = symbol;
        CandidateReason = CandidateReason.None;
        CandidateSymbols = symbol is Symbol value ? ImmutableArray.Create(value) : ImmutableArray<Symbol>.Empty;
    }

    internal SymbolInfo(Symbol? symbol, ImmutableArray<Symbol> candidateSymbols, CandidateReason candidateReason = CandidateReason.None) : this()
    {
        Symbol = symbol;
        CandidateSymbols = candidateSymbols;
        CandidateReason = candidateReason;
    }

    internal SymbolInfo(CandidateReason candidateReason, ImmutableArray<Symbol> candidateSymbols) : this()
    {
        CandidateReason = candidateReason;
        CandidateSymbols = candidateSymbols;
    }

    public CandidateReason CandidateReason { get; }

    public ImmutableArray<Symbol> CandidateSymbols { get; }

    public Symbol? Symbol { get; }

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
