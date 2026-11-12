using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundCarrierContractTests
{
    [Fact]
    public void WithExpressionVisitsReceiverThenAssignmentsAndReusesUnchangedState()
    {
        var type = Named("A", "Example.Record");
        var receiver = Literal(0, type);
        var value = Literal(1, type);
        var member = Make<PropertySymbol>(SymbolKind.Property, "Value");
        var clone = Make<MethodSymbol>(SymbolKind.Method, "Clone");
        var assignment = new BoundWithAssignment(member, value);
        var assignments = ImmutableArray.Create(assignment);
        var expression = new BoundWithExpression(receiver, assignments, BoundWithStrategyKind.RecordClone,
            null, [], [], clone, null, type);
        Assert.Same(clone, expression.Symbol);
        Assert.Same(expression, expression.Update(receiver, assignments, expression.Strategy, null,
            expression.MemberMethods, expression.ParameterMembers, clone, null, type, expression.Reason));
        Assert.Same(assignment, assignment.Update(member, value));
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal<BoundLiteralExpression>([receiver, value], walker.Values);
        walker.Values.Clear();
        walker.Visit(assignment);
        Assert.Same(value, Assert.Single(walker.Values));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(2)]
    public void CarrierAccessPreservesPayloadAndOptionalRuntimeMembers(int kind)
    {
        var type = Named("A", "Example.Payload");
        var receiver = Literal(0, type);
        var present = Literal(1, type);
        var local = Make<LocalSymbol>(SymbolKind.Local, "payload");
        var unwrap = Make<MethodSymbol>(SymbolKind.Method, "TryGetValue");
        var expression = new BoundCarrierConditionalAccessExpression(receiver, (BoundCarrierKind)kind, type, local, present, type,
            carrierType: type, resultTryGetValueForOkCaseMethod: unwrap, optionTryGetValueMethod: unwrap);
        Assert.Same(type, expression.Type);
        Assert.Same(type, expression.PayloadType);
        Assert.Same(local, expression.PayloadLocal);
        Assert.Same(unwrap, expression.ResultTryGetValueForOkCaseMethod);
        Assert.Same(unwrap, expression.OptionTryGetValueMethod);
        Assert.Null(expression.ResultErrorCtor);
        Assert.Null(expression.OptionNoneCtorOrFactory);
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal<BoundLiteralExpression>([receiver, present], walker.Values);
    }

    [Fact]
    public void TryAndPropagationRetainResultContractsAndVisitTheirOperand()
    {
        var type = Named("A", "Example.Result");
        var value = Literal(1, type);
        var ok = Make<MethodSymbol>(SymbolKind.Method, "Ok");
        var error = Make<MethodSymbol>(SymbolKind.Method, "Error");
        var attempt = new BoundTryExpression(value, type, type, ok, error);
        Assert.Same(ok, attempt.OkConstructor);
        Assert.Same(error, attempt.ErrorConstructor);
        Assert.Same(attempt, attempt.Update(value, type, type, ok, error));
        var propagation = new BoundPropagateExpression(value, type, null, type, error, "Some", "None", false);
        Assert.Same(type, propagation.OkType);
        Assert.Null(propagation.ErrorType);
        Assert.False(propagation.ErrorCaseHasPayload);
        Assert.Null(propagation.TryGetOutputMethod);
        Assert.Null(propagation.UnwrapErrorMethod);
        var walker = new RecordingWalker();
        walker.Visit(attempt);
        walker.Visit(propagation);
        Assert.Equal<BoundLiteralExpression>([value, value], walker.Values);
    }

    [Fact]
    public void PropagationPreservesProtocolMethodsAndClassifiedConversion()
    {
        var type = Named("A", "Example.Result");
        var value = Literal(1, type);
        var error = Make<MethodSymbol>(SymbolKind.Method, "Error");
        var unwrap = Make<MethodSymbol>(SymbolKind.Method, "Unwrap");
        var property = Make<PropertySymbol>(SymbolKind.Property, "Value");
        var conversion = new Conversion(isIdentity: true, isImplicit: true);
        var expression = new BoundPropagateExpression(value, type, type, type, error,
            "Success", "Failure", true, type, type, property, unwrap, conversion,
            BoundExpressionReason.OtherError, unwrap, error);
        Assert.Same(type, expression.ErrorType);
        Assert.Same(property, expression.OkValueProperty);
        Assert.Same(unwrap, expression.UnwrapErrorMethod);
        Assert.Same(unwrap, expression.TryGetOutputMethod);
        Assert.Same(error, expression.TryGetResidualMethod);
        Assert.True(expression.ErrorConversion.IsIdentity);
        Assert.Equal(BoundExpressionReason.OtherError, expression.Reason);
        Assert.Same(expression, expression.Update(value, type, type, type, error,
            "Success", "Failure", true, type, type, property, unwrap, conversion,
            BoundExpressionReason.OtherError, unwrap, error));
    }

    private static BoundLiteralExpression Literal(int value, TypeSymbol type)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Values { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add(node);
    }
}
