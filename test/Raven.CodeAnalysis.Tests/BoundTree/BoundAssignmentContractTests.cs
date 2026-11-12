using System.Reflection;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundAssignmentContractTests
{
    [Fact]
    public void StaticFieldAssignmentHasNoReceiverAndStillVisitsValue()
    {
        var type = Named("A", "Example.Value");
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", type);
        Set(field, "get_IsStatic", true);
        var value = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 7, type);
        var assignment = new BoundFieldAssignmentExpression(null, field, value, type);
        Assert.Null(assignment.Receiver);
        Assert.Null(assignment.Left);
        Assert.Same(value, assignment.Right);
        var walker = new RecordingWalker();
        walker.Visit(new BoundAssignmentStatement(assignment));
        Assert.Same(value, Assert.Single(walker.Values));
    }

    [Fact]
    public void BaseAssignmentLeftIsAnnotatedForTheMissingStaticReceiver()
    {
        var property = typeof(BoundAssignmentExpression).GetProperty(nameof(BoundAssignmentExpression.Left))!;
        Assert.Equal(NullabilityState.Nullable, new NullabilityInfoContext().Create(property).ReadState);
    }

    [Theory]
    [InlineData("local")]
    [InlineData("parameter")]
    [InlineData("property")]
    [InlineData("field")]
    [InlineData("array")]
    [InlineData("indexer")]
    [InlineData("member")]
    [InlineData("pattern")]
    [InlineData("byref")]
    public void AssignmentFamiliesVisitTheirStorageBeforeTheValue(string kind)
    {
        var type = Named("A", "Example.Value");
        var receiver = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, type);
        var index = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 2, type);
        var right = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 3, type);
        var local = Make<LocalSymbol>(SymbolKind.Local, "value");
        Set(local, "get_Type", type);
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Set(parameter, "get_Type", type);
        var property = Make<PropertySymbol>(SymbolKind.Property, "Item");
        Set(property, "get_Type", type);
        var field = Make<FieldSymbol>(SymbolKind.Field, "Value");
        Set(field, "get_Type", type);
        var localAccess = new BoundLocalAccess(local);
        var arrayAccess = new BoundArrayAccessExpression(receiver, [index], type);
        var indexerAccess = new BoundIndexerAccessExpression(receiver, [index], property);
        var pattern = new BoundDiscardPattern(type);
        BoundAssignmentExpression assignment = kind switch
        {
            "local" => new BoundLocalAssignmentExpression(local, localAccess, right, type),
            "parameter" => new BoundParameterAssignmentExpression(parameter, receiver, right, type),
            "property" => new BoundPropertyAssignmentExpression(receiver, property, receiver, right, type),
            "field" => new BoundFieldAssignmentExpression(receiver, field, right, type),
            "array" => new BoundArrayAssignmentExpression(arrayAccess, right, type),
            "indexer" => new BoundIndexerAssignmentExpression(indexerAccess, right, type),
            "member" => new BoundMemberAssignmentExpression(field, receiver, right, type),
            "pattern" => new BoundPatternAssignmentExpression(type, pattern, right, type),
            _ => new BoundByRefAssignmentExpression(receiver, type, right, type)
        };
        var walker = new OperandWalker();
        walker.Visit(assignment);
        BoundExpression[] operands = kind switch
        {
            "local" => [localAccess, right],
            "array" => [arrayAccess, receiver, index, right],
            "indexer" => [indexerAccess, receiver, index, right],
            "pattern" => [right],
            _ => [receiver, right]
        };
        Assert.Equal<BoundExpression>([assignment, .. operands], walker.Expressions);
        Assert.Equal(kind == "pattern" ? 1 : 0, walker.Patterns.Count);
        Assert.Same(type, assignment.UnitType);
        Assert.Same(type, assignment.Type);
        Assert.Same(right, assignment.Right);
        BoundAssignmentExpression Update(BoundExpression value) => assignment switch
        {
            BoundLocalAssignmentExpression node => node.Update(local, localAccess, value, type),
            BoundParameterAssignmentExpression node => node.Update(parameter, receiver, value, type),
            BoundPropertyAssignmentExpression node => node.Update(receiver, property, receiver, value, type),
            BoundFieldAssignmentExpression node => node.Update(receiver, field, value, type, false),
            BoundArrayAssignmentExpression node => node.Update(arrayAccess, value, type),
            BoundIndexerAssignmentExpression node => node.Update(indexerAccess, value, type),
            BoundMemberAssignmentExpression node => node.Update(field, receiver, value, type),
            BoundPatternAssignmentExpression node => node.Update(type, pattern, value, type),
            BoundByRefAssignmentExpression node => node.Update(receiver, type, value, type),
            _ => throw new InvalidOperationException()
        };
        Assert.Same(assignment, Update(right));
        var changed = Update(index);
        Assert.NotSame(assignment, changed);
        Assert.Same(index, changed.Right);
        Assert.Same(right, assignment.Right);
    }

    private sealed class OperandWalker : BoundTreeWalker
    {
        internal List<BoundExpression> Expressions { get; } = [];
        internal List<BoundPattern> Patterns { get; } = [];

        public override void VisitExpression(BoundExpression node)
        {
            Expressions.Add(node);
            base.VisitExpression(node);
        }

        public override void VisitDiscardPattern(BoundDiscardPattern node) => Patterns.Add(node);
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Values { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add(node);
    }
}
