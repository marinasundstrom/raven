using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundIterationTests
{
    [Fact]
    public void RangeFactoryPreservesOperandsAndRecordEquality()
    {
        var type = Named("A", "Example.Value");
        var start = Literal(0, type);
        var end = Literal(5, type);
        var step = Literal(1, type);
        var first = ForIterationInfo.ForRange(type, start, end, step, rangeUpperExclusive: true);
        var second = ForIterationInfo.ForRange(type, start, end, step, rangeUpperExclusive: true);
        Assert.Equal(first, second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.True(first == second);
        Assert.Equal(ForIterationKind.Range, first.Kind);
        Assert.Same(start, first.RangeStart);
        Assert.Same(end, first.RangeEnd);
        Assert.Same(step, first.RangeStep);
        Assert.Null(first.Range);
        Assert.Null(first.CancellationToken);
        Assert.True(first.RangeUpperExclusive);
        Assert.NotEqual(first, first with { RangeUpperExclusive = false });
    }

    [Fact]
    public void AsyncFactoryPreservesOptionalDisposalAndMethodRoles()
    {
        var type = Named("A", "Example.Value");
        var get = Make<MethodSymbol>(SymbolKind.Method, "GetAsyncEnumerator");
        var move = Make<MethodSymbol>(SymbolKind.Method, "MoveNextAsync");
        var current = Make<MethodSymbol>(SymbolKind.Method, "get_Current");
        var dispose = Make<MethodSymbol>(SymbolKind.Method, "DisposeAsync");
        var info = ForIterationInfo.ForAsync(type, get, move, current, dispose);
        Assert.Equal(ForIterationKind.Async, info.Kind);
        Assert.Same(type, info.ElementType);
        Assert.Same(get, info.GetEnumeratorMethod);
        Assert.Same(move, info.MoveNextAsyncMethod);
        Assert.Same(current, info.CurrentGetter);
        Assert.Same(dispose, info.DisposeAsyncMethod);
        Assert.Null(info.MoveNextMethod);
        Assert.Null(ForIterationInfo.ForAsync(type, get, move, current).DisposeAsyncMethod);
    }

    [Fact]
    public void ArrayFactoryUsesArrayElementType()
    {
        var type = Named("A", "Example.Value");
        var array = Make<ArrayTypeSymbol>(SymbolKind.Type, "Value[]", TypeKind.Array);
        Set(array, "get_ElementType", type);
        var info = ForIterationInfo.ForArray(array);
        Assert.Equal(ForIterationKind.Array, info.Kind);
        Assert.Same(type, info.ElementType);
        Assert.Same(array, info.ArrayType);
        Assert.Null(info.GetEnumeratorMethod);
    }

    [Fact]
    public void EnumerableFactoriesPreserveMethodRolesAndOptionalMetadata()
    {
        var type = Named("A", "Example.Value");
        var enumerable = Named("A", "Example.IEnumerable");
        var enumerator = Named("A", "Example.IEnumerator");
        Set(enumerable, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(type));
        var get = Make<MethodSymbol>(SymbolKind.Method, "GetEnumerator");
        var move = Make<MethodSymbol>(SymbolKind.Method, "MoveNext");
        var current = Make<MethodSymbol>(SymbolKind.Method, "get_Current");
        var generic = ForIterationInfo.ForGeneric(enumerable, enumerator, get, move, current);
        Assert.Equal(ForIterationKind.Generic, generic.Kind);
        Assert.Same(type, generic.ElementType);
        Assert.Same(enumerable, generic.EnumerableInterface);
        Assert.Same(enumerator, generic.EnumeratorInterface);
        Assert.Same(get, generic.GetEnumeratorMethod);
        Assert.Same(move, generic.MoveNextMethod);
        Assert.Same(current, generic.CurrentGetter);
        Assert.Null(generic.MoveNextAsyncMethod);
        var nonGeneric = ForIterationInfo.ForNonGeneric(type, get, move, current);
        Assert.Equal(ForIterationKind.NonGeneric, nonGeneric.Kind);
        Assert.Same(get, nonGeneric.GetEnumeratorMethod);
        Assert.Same(move, nonGeneric.MoveNextMethod);
        Assert.Same(current, nonGeneric.CurrentGetter);
        Assert.Null(nonGeneric.EnumerableInterface);
        Assert.Null(ForIterationInfo.ForNonGeneric(type).GetEnumeratorMethod);
    }

    [Fact]
    public void ForAndYieldKeepIterationMetadataAndVisitTheirOperands()
    {
        var type = Named("A", "Example.Value");
        var collection = Literal(0, type);
        var value = Literal(1, type);
        var iteration = ForIterationInfo.ForNonGeneric(type);
        var yield = new BoundYieldStatement(value, type, IteratorMethodKind.Enumerable, iteration);
        var loop = new BoundForStatement(null, iteration, collection, yield);
        Assert.Same(iteration, loop.Iteration);
        Assert.Same(iteration, yield.Iteration);
        Assert.Same(loop, loop.Update(null, iteration, collection, yield));
        var walker = new RecordingWalker();
        walker.Visit(loop);
        var expression = new BoundYieldExpression(value, type, IteratorMethodKind.Enumerable, type);
        walker.Visit(expression);
        Assert.Equal<BoundLiteralExpression>([collection, value, value], walker.Values);
        Assert.Null(expression.Iteration);
        var error = Assert.Throws<ArgumentNullException>(() => new BoundYieldStatement(null!, type, IteratorMethodKind.Enumerable));
        Assert.Equal("expression", error.ParamName);
    }

    private static BoundLiteralExpression Literal(int value, TypeSymbol type)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<BoundLiteralExpression> Values { get; } = [];

        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add(node);
    }
}
