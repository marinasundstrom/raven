using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundIfExpressionContractTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void EqualOrMissingElseTypePreservesThenType(bool includeElse)
    {
        var type = Type("Value");
        var condition = Literal(type, 0);
        var thenBranch = Literal(type, 1);
        var elseBranch = includeElse ? Literal(type, 2) : null;
        var node = new BoundIfExpression(condition, thenBranch, elseBranch);
        Assert.Same(type, node.Type);
        Assert.Same(condition, node.Condition);
        Assert.Same(thenBranch, node.ThenBranch);
        Assert.Same(elseBranch, node.ElseBranch);
        Assert.Null(node.Symbol);
        Assert.Equal(BoundExpressionReason.None, node.Reason);
    }

    [Fact]
    public void UntypedThenUsesElseTypeAndBothUntypedAreRejected()
    {
        var type = Type("Value");
        var untyped = new BoundNamespaceExpression(Make<NamespaceSymbol>(SymbolKind.Namespace, "Example"));
        var condition = Literal(type, 0);
        Assert.Same(type, new BoundIfExpression(condition, untyped, Literal(type, 1)).Type);
        Assert.Same(type, new BoundIfExpression(condition, Literal(type, 1), untyped).Type);
        Assert.Equal("If expression must have a type.", Assert.Throws<InvalidOperationException>(() =>
            new BoundIfExpression(condition, untyped)).Message);
    }

    [Fact]
    public void DistinctBranchesUseCommonInferenceAndUpdateRecomputesType()
    {
        var parent = Type("Parent");
        var left = Type("Left", parent);
        var right = Type("Right", parent);
        var condition = Literal(Type("Boolean"), 0);
        var thenBranch = Literal(left, 1);
        var elseBranch = Literal(right, 2);
        var node = new BoundIfExpression(condition, thenBranch, elseBranch);
        Assert.Same(parent, node.Type);
        Assert.Same(node, node.Update(condition, thenBranch, elseBranch));
        var changed = node.Update(condition, thenBranch, thenBranch);
        Assert.NotSame(node, changed);
        Assert.Same(left, changed.Type);
        Assert.Same(thenBranch, changed.ElseBranch);
        Assert.Same(left, node.Update(condition, thenBranch, null).Type);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void WalkerVisitsConditionThenBranchesInOrder(bool includeElse)
    {
        var type = Type("Value");
        var walker = new RecordingWalker();
        walker.Visit(new BoundIfExpression(Literal(type, 0), Literal(type, 1), includeElse ? Literal(type, 2) : null));
        Assert.Equal(includeElse ? [0, 1, 2] : new[] { 0, 1 }, walker.Values);
    }

    [Fact]
    public void GenericVisitorDispatchesToIfExpression()
    {
        var type = Type("Value");
        Assert.Equal(42, new ResultVisitor().Visit(new BoundIfExpression(Literal(type, 0), Literal(type, 1))));
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        public List<int> Values { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add((int)node.Value);
    }

    private sealed class ResultVisitor : BoundTreeVisitor<int>
    {
        public override int VisitIfExpression(BoundIfExpression node) => 42;
    }

    private static BoundLiteralExpression Literal(TypeSymbol type, int value) => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private static NamedTypeSymbol Type(string name, NamedTypeSymbol? parent = null)
    {
        var type = Make<NamedTypeSymbol>(SymbolKind.Type, name, TypeKind.Class);
        Set(type, "get_BaseType", parent);
        Set(type, "get_IsUnion", false);
        Set(type, "get_IsUnionCase", false);
        Set(type, "get_IsNullable", false);
        Set(type, "get_IsValueType", false);
        Set(type, "get_AllInterfaces", ImmutableArray<NamedTypeSymbol>.Empty);
        return type;
    }
}
