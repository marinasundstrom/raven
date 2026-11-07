using System.Reflection;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "DotNet")]
public sealed class BoundHierarchyContractTests
{
    [Fact]
    public void BoundFamiliesDeclareTheirDirectChildrenAndLeavesAreFinal()
    {
        var nodes = typeof(BoundNode).Assembly.GetTypes()
            .Where(type => typeof(BoundNode).IsAssignableFrom(type)).ToArray();
        foreach (var node in nodes)
        {
            var children = nodes.Where(type => type.BaseType == node).ToArray();
            if (children.Length == 0)
            {
                Assert.True(node.IsSealed, $"{node.Name} must be a final leaf.");
                continue;
            }

            Assert.True(node.IsAbstract, $"{node.Name} must be an abstract family.");
            var closure = Assert.Single(node.GetCustomAttributesData(), attribute =>
                attribute.AttributeType.FullName == "System.Runtime.CompilerServices.IsClosedTypeAttribute");
            var types = Assert.Single(closure.NamedArguments, argument => argument.MemberName == "DerivedTypes");
            var declared = Assert.IsAssignableFrom<IEnumerable<CustomAttributeTypedArgument>>(types.TypedValue.Value)
                .Select(argument => Assert.IsAssignableFrom<Type>(argument.Value)).ToArray();
            Assert.Equal(children.OrderBy(type => type.FullName), declared.OrderBy(type => type.FullName));
        }
    }

    [Fact]
    public void ExhaustiveDispatchPreservesNullDefaultVisits()
    {
        var visitor = new CountingVisitor();
        visitor.VisitStatement(null!);
        visitor.VisitExpression(null!);
        visitor.VisitPattern(null!);
        visitor.VisitDesignator(null!);
        Assert.Equal(4, visitor.Count);

        var results = new ResultVisitor();
        Assert.Equal(42, results.VisitStatement(null!));
        Assert.Equal(42, results.VisitExpression(null!));
        Assert.Equal(42, results.VisitPattern(null!));
        Assert.Equal(42, results.VisitDesignator(null!));

        var walker = new CountingWalker();
        walker.Visit(null!);
        Assert.Equal(1, walker.Count);
    }

    private sealed class CountingVisitor : BoundTreeVisitor
    {
        public int Count { get; private set; }
        public override void DefaultVisit(BoundNode node) => Count++;
    }

    private sealed class ResultVisitor : BoundTreeVisitor<int>
    {
        public override int DefaultVisit(BoundNode node) => 42;
    }

    private sealed class CountingWalker : BoundTreeWalker
    {
        public int Count { get; private set; }
        public override void DefaultVisit(BoundNode node) => Count++;
    }

    [Theory]
    [InlineData("BoundTreeVisitor")]
    [InlineData("BoundTreeVisitor`1")]
    [InlineData("BoundTreeWalker")]
    public void VisitorsAndWalkersRemainExtensible(string name)
    {
        var visitor = typeof(BoundNode).Assembly.GetType("Raven.CodeAnalysis." + name, throwOnError: true)!;
        Assert.False(visitor.IsSealed);
    }
}
