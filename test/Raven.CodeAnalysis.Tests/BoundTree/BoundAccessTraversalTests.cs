using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundAccessTraversalTests
{
    [Theory]
    [InlineData("allocation")]
    [InlineData("index")]
    [InlineData("range")]
    [InlineData("dereference")]
    public void WalkerReachesAllocationIndexRangeAndDereferenceOperands(string kind)
    {
        var type = Named("A", "Value");
        var literal = Literal(7, type);
        BoundExpression expression = kind switch
        {
            "allocation" => new BoundArrayCreationExpression(Make<ArrayTypeSymbol>(SymbolKind.Type, "Array", TypeKind.Array), literal),
            "index" => new BoundIndexExpression(literal, true, type),
            "range" => new BoundRangeExpression(new BoundIndexExpression(literal, false, type), null, type),
            _ => new BoundDereferenceExpression(literal, type)
        };
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Same(literal, Assert.Single(walker.Literals));
    }

    [Fact]
    public void IndexerCapturesPrecedeReceiverAndArgumentsAndAreSnapshotted()
    {
        var type = Named("A", "Value");
        var capture = Literal(1, type);
        var receiver = Literal(2, type);
        var argument = Literal(3, type);
        var indexer = Make<PropertySymbol>(SymbolKind.Property, "Item");
        Set(indexer, "get_Type", type);
        var captures = new List<BoundStatement> { new BoundExpressionStatement(capture) };
        var access = new BoundIndexerAccessExpression(receiver, [argument], indexer, captures);
        captures.Clear();
        var walker = new RecordingWalker();
        walker.Visit(access);
        Assert.Equal<BoundLiteralExpression>([capture, receiver, argument], walker.Literals);
        Assert.Same(indexer, access.Symbol);
        Assert.Same(type, access.Type);
        Assert.Same(access, access.Update(receiver, access.Arguments, indexer, access.Captures));
        Assert.Empty(new BoundIndexerAccessExpression(receiver, [], indexer).Captures);
    }

    [Fact]
    public void ArrayTupleAndAwaitTraversalPreservesOperandOrderAndSymbols()
    {
        var type = Named("A", "Value");
        var receiver = Literal(1, type);
        var index = Literal(2, type);
        var operand = Literal(3, type);
        var getAwaiter = Make<MethodSymbol>(SymbolKind.Method, "GetAwaiter");
        var getResult = Make<MethodSymbol>(SymbolKind.Method, "GetResult");
        var completed = Make<PropertySymbol>(SymbolKind.Property, "IsCompleted");
        var awaited = new BoundAwaitExpression(operand, type, type, getAwaiter, getResult, completed);
        var array = new BoundArrayAccessExpression(receiver, [index], type);
        var tuple = new BoundTupleExpression([array, awaited], type);
        var walker = new RecordingWalker();
        walker.Visit(tuple);
        Assert.Equal<BoundLiteralExpression>([receiver, index, operand], walker.Literals);
        Assert.Same(type, tuple.Type);
        Assert.Same(type, array.ElementType);
        Assert.Same(type, awaited.ResultType);
        Assert.Same(getAwaiter, awaited.GetAwaiterMethod);
        Assert.Same(getResult, awaited.GetResultMethod);
        Assert.Same(completed, awaited.IsCompletedProperty);
        Assert.Same(tuple, tuple.Update(tuple.Elements, type));
        var changedType = Named("A", "Other");
        Assert.Same(changedType, tuple.Update(tuple.Elements, changedType).Type);
    }

    [Fact]
    public void PropertyRangesAndDereferencesRetainTheirContracts()
    {
        var type = Named("A", "Value");
        var property = Make<PropertySymbol>(SymbolKind.Property, "Value");
        Set(property, "get_Type", type);
        var access = new BoundPropertyAccess(property, BoundExpressionReason.Inaccessible);
        Assert.Same(property, access.Symbol);
        Assert.Same(type, access.Type);
        Assert.Equal(BoundExpressionReason.Inaccessible, access.Reason);
        var reference = Literal(4, type);
        var dereference = new BoundDereferenceExpression(reference, type);
        Assert.Same(reference, dereference.Reference);
        Assert.Same(type, dereference.ElementType);
        Assert.Same(property, new BoundDereferenceExpression(access, type).Symbol);
        Assert.Equal("reference", Assert.Throws<ArgumentNullException>(() => new BoundDereferenceExpression(null!, type)).ParamName);
        Assert.Equal("elementType", Assert.Throws<ArgumentNullException>(() => new BoundDereferenceExpression(reference, null!)).ParamName);
        var end = new BoundIndexExpression(reference, true, type);
        var range = new BoundRangeExpression(null, end, type, isUpperExclusive: false);
        Assert.True(end.IsFromEnd);
        Assert.False(range.IsUpperExclusive);
        Assert.Same(end, range.Right);
        Assert.Null(range.Left);
        Assert.Same(range, range.Update(null, end, type, false));
        var walker = new RecordingWalker();
        walker.Visit(range);
        walker.Visit(new BoundRangeExpression(null, null, type));
        Assert.Same(reference, Assert.Single(walker.Literals));
        var start = Literal(1, type);
        walker.Visit(new BoundRangeExpression(new BoundIndexExpression(start, false, type), end, type));
        Assert.Equal<BoundLiteralExpression>([reference, start, reference], walker.Literals);
    }

    private static BoundLiteralExpression Literal(int value, TypeSymbol type)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Literals { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Literals.Add(node);
    }
}
