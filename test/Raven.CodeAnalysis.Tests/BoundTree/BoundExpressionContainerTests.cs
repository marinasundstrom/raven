using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundExpressionContainerTests
{
    [Fact]
    public void BlockExpressionsSnapshotOnceAndSelectTheirFinalValueType()
    {
        var unit = Named("A", "Example.Unit");
        var type = Named("A", "Example.Value");
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 3, type);
        var calls = 0;
        IEnumerable<BoundStatement> Statements()
        {
            calls++;
            yield return new BoundExpressionStatement(literal);
        }
        var block = new BoundBlockExpression(Statements(), unit, introduceILScope: false);
        Assert.Equal(1, calls);
        Assert.Same(type, block.Type);
        Assert.Same(unit, block.UnitType);
        Assert.Single(block.Statements);
        Assert.Single(block.Statements);
        Assert.Equal(1, calls);
        Assert.False(block.IntroduceILScope);
        Assert.True(block.LocalsToDispose.IsEmpty);
        Assert.Same(block, block.Update(block.Statements, unit, block.LocalsToDispose, false));
        Assert.Same(unit, new BoundBlockExpression(null, unit).Type);
        var cleared = block.Update(null, unit, default, true);
        Assert.Empty(cleared.Statements);
        Assert.Same(unit, cleared.Type);
        Assert.Same(unit, new BoundBlockExpression([new BoundReturnStatement(literal)], unit).Type);
        BoundStatement[] input = [new BoundExpressionStatement(literal)];
        var snapshot = new BoundBlockExpression(input, unit);
        input[0] = new BoundBreakStatement();
        Assert.IsType<BoundExpressionStatement>(Assert.Single(snapshot.Statements));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void WalkerVisitsOperandsOfNullableAndBaseReceivers(bool nullableValue)
    {
        var type = Named("A", "Example.Value");
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 7, type);
        BoundExpression wrapper = nullableValue
            ? new BoundNullableValueExpression(literal, type)
            : new BoundBaseExpression(type, receiver: literal);
        var walker = new RecordingWalker();
        walker.Visit(wrapper);
        Assert.Same(literal, Assert.Single(walker.Literals));
    }

    [Fact]
    public void WalkerTraversesExpressionBranchesWithoutDuplicatingConditionalReceiver()
    {
        var type = Named("A", "Example.Value");
        var first = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, type);
        var second = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 2, type);
        var placeholder = new BoundConditionalReceiverExpression(type);
        var conditional = new BoundConditionalAccessExpression(first, new BoundNullCoalesceExpression(placeholder, second, type), type);
        var body = new BoundBlockExpression([
            new BoundExpressionStatement(new BoundReturnExpression(conditional, type)),
            new BoundExpressionStatement(new BoundThrowExpression(second, type)),
            new BoundExpressionStatement(new BoundReturnExpression(null, type)),
            new BoundExpressionStatement(new BoundBreakExpression(null, type)),
            new BoundExpressionStatement(new BoundContinueExpression(null, type)),
            new BoundExpressionStatement(new BoundBaseExpression(type))
        ], type);
        var walker = new RecordingWalker();
        walker.Visit(body);
        Assert.Equal<BoundLiteralExpression>([first, second, second], walker.Literals);
        Assert.Same(conditional, conditional.Update(first, conditional.WhenNotNull, type));
        Assert.Same(placeholder, placeholder.Update(type));
    }

    [Fact]
    public void SymbolExpressionsPreserveTheReferencedIdentityAndReason()
    {
        var type = Named("A", "Example.Value");
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Set(parameter, "get_Type", type);
        var access = new BoundParameterAccess(parameter, BoundExpressionReason.Inaccessible);
        Assert.Same(parameter, access.Symbol);
        Assert.Same(type, access.Type);
        Assert.Equal(BoundExpressionReason.Inaccessible, access.Reason);
        Assert.Same(access, access.Update(parameter, access.Reason));
        var self = new BoundSelfExpression(type, BoundExpressionReason.OtherError);
        Assert.Same(type, self.Symbol);
        Assert.Equal(BoundExpressionReason.OtherError, self.Reason);
        var typeExpression = new BoundTypeExpression(type);
        Assert.Same(type, typeExpression.TypeSymbol);
        Assert.Same(type, typeExpression.Symbol);
        var ns = Make<NamespaceSymbol>(SymbolKind.Namespace, "Example");
        var namespaceExpression = new BoundNamespaceExpression(ns);
        Assert.Same(ns, namespaceExpression.Symbol);
        Assert.Null(namespaceExpression.Type);
        var name = new BoundNameOfExpression(parameter, type);
        Assert.Equal("value", name.Name);
        Assert.Equal("value", name.ToString());
        Assert.Same(parameter, name.Symbol);
        var typeOf = new BoundTypeOfExpression(type, type);
        Assert.Same(type, typeOf.OperandType);
        Assert.Null(typeOf.Symbol);
    }

    [Fact]
    public void ExpressionTypeMetadataAllowsIntentionalAbsence()
    {
        var context = new System.Reflection.NullabilityInfoContext();
        var property = typeof(BoundExpression).GetProperty(nameof(BoundExpression.Type))!;
        Assert.Equal(System.Reflection.NullabilityState.Nullable, context.Create(property).ReadState);
        var constructor = Assert.Single(typeof(BoundExpression).GetConstructors(
            System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic));
        Assert.Equal(System.Reflection.NullabilityState.Nullable, context.Create(constructor.GetParameters()[0]).ReadState);
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Literals { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals.Add(node);
    }
}
