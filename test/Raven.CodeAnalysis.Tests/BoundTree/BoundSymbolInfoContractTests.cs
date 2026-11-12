using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundSymbolInfoContractTests
{
    [Fact]
    public void EveryBindingReasonMapsToItsPublicCandidateReason()
    {
        var expected = new[]
        {
            CandidateReason.None, CandidateReason.NotFound, CandidateReason.OverloadResolutionFailure,
            CandidateReason.Ambiguous, CandidateReason.Inaccessible, CandidateReason.WrongArity,
            CandidateReason.None, CandidateReason.NotFound, CandidateReason.None,
            CandidateReason.None, CandidateReason.None, CandidateReason.OverloadResolutionFailure
        };
        var reasons = Enum.GetValues<BoundExpressionReason>();
        Assert.Equal(expected.Length, reasons.Length);
        for (var i = 0; i < reasons.Length; i++)
            Assert.Equal(expected[i], BoundExpressionExtensions.Convert(reasons[i]));
        var error = Assert.Throws<ArgumentOutOfRangeException>(() => BoundExpressionExtensions.Convert((BoundExpressionReason)999));
        Assert.Equal("reason", error.ParamName);
        Assert.Equal((BoundExpressionReason)999, error.ActualValue);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ErrorPreservesExplicitCandidatesAndOnlyPublishesSuccessfulSymbol(bool failed)
    {
        var symbol = Make<MethodSymbol>(SymbolKind.Method, "Selected");
        var candidate = Make<MethodSymbol>(SymbolKind.Method, "Candidate");
        var node = new BoundErrorExpression(Type(), symbol,
            failed ? BoundExpressionReason.Ambiguous : BoundExpressionReason.None, [candidate]);
        var info = BoundExpressionExtensions.GetSymbolInfo(node);
        Assert.Same(failed ? null : symbol, info.Symbol);
        Assert.Same(candidate, Assert.Single(info.CandidateSymbols));
        Assert.Equal(failed ? CandidateReason.Ambiguous : CandidateReason.None, info.CandidateReason);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EmptyErrorCandidatesFallBackToAvailableSymbol(bool hasSymbol)
    {
        var symbol = hasSymbol ? Make<MethodSymbol>(SymbolKind.Method, "Available") : null;
        var info = BoundExpressionExtensions.GetSymbolInfo(new BoundErrorExpression(Type(), symbol, BoundExpressionReason.NotFound));
        Assert.Null(info.Symbol);
        Assert.Equal(CandidateReason.NotFound, info.CandidateReason);
        Assert.Equal<Symbol>(hasSymbol ? [symbol!] : ImmutableArray<Symbol>.Empty, info.CandidateSymbols);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void MethodGroupsAndDelegateCreationKeepCandidatesAndSelection(bool failed)
    {
        var first = Make<MethodSymbol>(SymbolKind.Method, "First");
        var selected = Make<MethodSymbol>(SymbolKind.Method, "Second");
        var group = new BoundMethodGroupExpression(null, [first, selected], Type(), selectedMethod: selected,
            reason: failed ? BoundExpressionReason.OverloadResolutionFailed : BoundExpressionReason.None);
        var groupInfo = BoundExpressionExtensions.GetSymbolInfo(group);
        var delegateInfo = BoundExpressionExtensions.GetSymbolInfo(new BoundDelegateCreationExpression(group, Type()));
        Assert.Same(failed ? null : selected, groupInfo.Symbol);
        Assert.Same(failed ? null : selected, delegateInfo.Symbol);
        Assert.Equal(new Symbol[] { first, selected }, groupInfo.CandidateSymbols);
        Assert.Equal<Symbol>(groupInfo.CandidateSymbols, delegateInfo.CandidateSymbols);
        Assert.Equal(failed ? CandidateReason.OverloadResolutionFailure : CandidateReason.MemberGroup, groupInfo.CandidateReason);
        Assert.Equal(failed ? CandidateReason.OverloadResolutionFailure : CandidateReason.None, delegateInfo.CandidateReason);
    }

    [Fact]
    public void MultipleMethodCandidatesRemainUnselected()
    {
        var first = Make<MethodSymbol>(SymbolKind.Method, "First");
        var second = Make<MethodSymbol>(SymbolKind.Method, "Second");
        var group = new BoundMethodGroupExpression(null, [first, second], Type());
        var info = BoundExpressionExtensions.GetSymbolInfo(group);
        Assert.Null(info.Symbol);
        Assert.Equal(CandidateReason.MemberGroup, info.CandidateReason);
        Assert.Equal(new Symbol[] { first, second }, info.CandidateSymbols);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void OrdinaryExpressionsPublishOnlySuccessfulSymbols(bool failed)
    {
        var local = Make<LocalSymbol>(SymbolKind.Local, "local");
        Set(local, "get_Type", Type());
        var node = new BoundLocalAccess(local, failed ? BoundExpressionReason.Inaccessible : BoundExpressionReason.None);
        var info = BoundExpressionExtensions.GetSymbolInfo(node);
        Assert.Same(failed ? null : local, info.Symbol);
        Assert.Same(local, Assert.Single(info.CandidateSymbols));
        Assert.Equal(failed ? CandidateReason.Inaccessible : CandidateReason.None, info.CandidateReason);
    }

    [Fact]
    public void OrdinaryFailedExpressionWithoutSymbolKeepsReasonAndEmptyCandidates()
    {
        var info = BoundExpressionExtensions.GetSymbolInfo(new BoundUnitExpression(Type(), BoundExpressionReason.MissingType));
        Assert.Null(info.Symbol);
        Assert.Empty(info.CandidateSymbols);
        Assert.Equal(CandidateReason.NotFound, info.CandidateReason);
    }

    [Fact]
    public void ExpressionsAndStatementsWithoutSymbolsYieldEmptyInformation()
    {
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, Type());
        var expression = BoundExpressionExtensions.GetSymbolInfo(literal);
        var statement = BoundExpressionExtensions.GetSymbolInfo(new BoundExpressionStatement(literal));
        Assert.Null(expression.Symbol);
        Assert.Empty(expression.CandidateSymbols);
        Assert.Equal(CandidateReason.None, expression.CandidateReason);
        Assert.Equal(expression, statement);
    }

    private static NamedTypeSymbol Type() => Named("A", "Example.Value");
}
