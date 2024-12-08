using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

public struct SymbolInfo
{
    internal SymbolInfo(ISymbol symbol) : this()
    {
        Symbol = symbol;
    }

    internal SymbolInfo(CandidateReason candidateReason, ImmutableArray<ISymbol> candidateSymbols) : this()
    {
        CandidateReason = candidateReason;
        CandidateSymbols = candidateSymbols;
    }

    public CandidateReason CandidateReason { get; }

    public ImmutableArray<ISymbol> CandidateSymbols { get; }

    public ISymbol? Symbol { get; }
}
