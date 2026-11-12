using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundNodeFactoryContractTests
{
    [Fact]
    public void MissingTypeProviderIsRejectedAtConstruction()
    {
        Assert.Equal("types", Assert.Throws<ArgumentNullException>(() => new BoundNodeFactory(null!)).ParamName);
    }

    [Fact]
    public void SynthesizedUnitAndAssignmentNodesUseProviderIdentity()
    {
        var types = new Types();
        var factory = new BoundNodeFactory(types);
        var value = factory.CreateLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 7, types.ErrorTypeSymbol);
        var local = Make<LocalSymbol>(SymbolKind.Local, "local");
        Set(local, "get_Type", types.ErrorTypeSymbol);
        var left = factory.CreateLocalAccess(local);
        var assignment = factory.CreateLocalAssignmentExpression(local, left, value);
        Assert.Same(types.UnitTypeSymbol, assignment.Type);
        Assert.Same(left, assignment.Left);
        Assert.Same(value, assignment.Right);
        Assert.Same(local, assignment.Local);
        var unit = factory.UnitExpression(BoundExpressionReason.OtherError);
        Assert.Same(types.UnitTypeSymbol, unit.Type);
        Assert.Equal(BoundExpressionReason.OtherError, unit.Reason);
        Assert.Same(types.UnitTypeSymbol, factory.CreateBlockExpression([]).Type);
    }

    [Fact]
    public void ErrorConvenienceUsesFallbackWithoutLosingCandidatesOrReason()
    {
        var types = new Types();
        var factory = new BoundNodeFactory(types);
        var symbol = Make<MethodSymbol>(SymbolKind.Method, "Candidate");
        var error = factory.ErrorExpression(symbol: symbol, reason: BoundExpressionReason.Ambiguous, candidates: [symbol]);
        Assert.Same(types.ErrorTypeSymbol, error.Type);
        Assert.Same(symbol, error.Symbol);
        Assert.Same(symbol, Assert.Single(error.Candidates));
        Assert.Equal(BoundExpressionReason.Ambiguous, error.Reason);
        Assert.Same(types.NullTypeSymbol, factory.ErrorExpression(types.NullTypeSymbol).Type);
        Assert.Empty(factory.ErrorExpression().Candidates);
    }

    [Fact]
    public void NullLiteralPreservesIntentionalNullAndConversionType()
    {
        var types = new Types();
        var factory = new BoundNodeFactory(types);
        var literal = factory.NullLiteral(types.UnitTypeSymbol);
        Assert.Equal(BoundLiteralExpressionKind.NullLiteral, literal.Kind);
        Assert.Null(literal.Value);
        Assert.Same(types.NullTypeSymbol, literal.GetNaturalType());
        Assert.Same(types.UnitTypeSymbol, literal.GetConvertedType());
        Assert.Same(literal, literal.Update(literal.Kind, null, types.NullTypeSymbol, types.UnitTypeSymbol));
        Assert.Null(factory.NullLiteral().GetConvertedType());
    }

    [Fact]
    public void StaticPropertyAssignmentRetainsAbsentReceiverAndLeft()
    {
        var types = new Types();
        var factory = new BoundNodeFactory(types);
        var property = Make<PropertySymbol>(SymbolKind.Property, "Value");
        var value = factory.NullLiteral();
        var assignment = factory.CreatePropertyAssignmentExpression(null, property, value);
        Assert.Null(assignment.Receiver);
        Assert.Null(assignment.Left);
        Assert.Same(property, assignment.Property);
        Assert.Same(value, assignment.Right);
        Assert.Same(types.UnitTypeSymbol, assignment.Type);
        Assert.Same(assignment, assignment.Update(null, property, null, value, types.UnitTypeSymbol));
    }

    [Fact]
    public void MethodGroupConveniencePreservesSelectionAndLazyDelegateFactory()
    {
        var types = new Types();
        var factory = new BoundNodeFactory(types);
        var first = Make<MethodSymbol>(SymbolKind.Method, "First");
        var second = Make<MethodSymbol>(SymbolKind.Method, "Second");
        var calls = 0;
        var group = factory.MethodGroupExpression(null, [first, second], () => { calls++; return types.UnitTypeSymbol; }, second);
        Assert.Same(types.ErrorTypeSymbol, group.MethodGroupType);
        Assert.Same(second, group.SelectedMethod);
        Assert.Equal(new[] { first, second }, group.Methods);
        Assert.Equal(0, calls);
        Assert.Same(types.UnitTypeSymbol, group.DelegateType);
        Assert.Same(types.UnitTypeSymbol, group.DelegateType);
        Assert.Equal(1, calls);
    }

    [Fact]
    public void FunctionAndConditionalFactoriesPreserveTheirInputs()
    {
        var types = new Types();
        var factory = new BoundNodeFactory(types);
        var method = Make<MethodSymbol>(SymbolKind.Method, "Run");
        var body = factory.CreateLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, types.UnitTypeSymbol);
        Assert.Same(method, factory.CreateFunctionStatement(method).Method);
        var function = factory.CreateFunctionExpression([], types.UnitTypeSymbol, body, method, types.UnitTypeSymbol, [], []);
        Assert.Same(body, function.Body);
        Assert.Same(method, function.Symbol);
        var conditional = factory.CreateIfExpression(body, body);
        Assert.Same(body, conditional.Condition);
        Assert.Same(body, conditional.ThenBranch);
        Assert.Null(conditional.ElseBranch);
    }

    private sealed class Types : BoundNodeTypeProvider
    {
        public NamedTypeSymbol UnitTypeSymbol { get; } = Named("A", "Example.Unit");
        public TypeSymbol NullTypeSymbol { get; } = Named("A", "Example.Null");
        public TypeSymbol ErrorTypeSymbol { get; } = Named("A", "Example.Error");
    }
}
