using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SymbolInfoValueTests
{
    [Fact]
    public void EmptyAndDefaultResultsPreserveTheirStorageAndDisplay()
    {
        var empty = SymbolInfo.None;
        Assert.False(empty.Success);
        Assert.Null(empty.Symbol);
        Assert.Equal(CandidateReason.None, empty.CandidateReason);
        Assert.False(empty.CandidateSymbols.IsDefault);
        Assert.Empty(empty.CandidateSymbols);
        Assert.Equal("None", empty.ToString());
        Assert.True(default(SymbolInfo).CandidateSymbols.IsDefault);
        Assert.Equal("None", default(SymbolInfo).ToString());
        Assert.NotEqual(default(SymbolInfo), empty);
    }

    [Fact]
    public void CandidateReasonAndArrayIdentityRemainPartOfEquality()
    {
        // Reusing an immutable array preserves the existing field-value contract.
        var candidates = ImmutableArray.Create<Symbol>((Symbol)null!);
        var left = new SymbolInfo(CandidateReason.Ambiguous, candidates);
        var same = new SymbolInfo(null, candidates, CandidateReason.Ambiguous);
        var otherReason = new SymbolInfo(CandidateReason.Inaccessible, candidates);
        var otherArray = new SymbolInfo(CandidateReason.Ambiguous, ImmutableArray.Create<Symbol>((Symbol)null!));
        Assert.Equal(left, same);
        Assert.True(left == same);
        Assert.True(left != otherReason);
        Assert.Equal(left.GetHashCode(), same.GetHashCode());
        Assert.NotEqual(left, otherReason);
        Assert.NotEqual(left, otherArray);
        Assert.False(left.Success);
        Assert.Equal("Candidate:  (Reason: Ambiguous)", left.ToString());
    }

    [Fact]
    public void NullSymbolConstructorProducesAnEmptyResult()
    {
        Assert.Equal(SymbolInfo.None, new SymbolInfo((Symbol?)null));
    }

    [Fact]
    public void TypedEqualityMatchesObjectEquality()
    {
        var value = new SymbolInfo(CandidateReason.NotFound, ImmutableArray<Symbol>.Empty);
        var equality = Assert.IsAssignableFrom<IEquatable<SymbolInfo>>(value);
        Assert.True(equality.Equals(new SymbolInfo(CandidateReason.NotFound, ImmutableArray<Symbol>.Empty)));
        Assert.False(equality.Equals(SymbolInfo.None));
        Assert.False(value.Equals((object?)null));
        Assert.False(value.Equals("None"));
    }
}
