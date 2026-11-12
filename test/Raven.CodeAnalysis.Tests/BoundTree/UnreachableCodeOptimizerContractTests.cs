using System.Collections.Immutable;

using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class UnreachableCodeOptimizerContractTests
{
    [Theory]
    [InlineData("return")]
    [InlineData("throw")]
    [InlineData("break")]
    [InlineData("continue")]
    public void TerminalTransfersRemoveFollowingStatementsAndPreserveBlockMetadata(string kind)
    {
        BoundStatement terminal = kind switch
        {
            "throw" => new BoundThrowStatement(Value()),
            "break" => new BoundBreakStatement(),
            "continue" => new BoundContinueStatement(),
            _ => new BoundReturnStatement(null),
        };
        var local = Make<LocalSymbol>(SymbolKind.Local, "resource");
        var block = new BoundBlockStatement([terminal, Statement()], [local], introduceILScope: false);
        var result = Rewrite(block);
        Assert.Same(terminal, Assert.Single(result.Statements));
        Assert.Same(local, Assert.Single(result.LocalsToDispose));
        Assert.False(result.IntroduceILScope);
    }

    [Theory]
    [InlineData("goto")]
    [InlineData("break")]
    [InlineData("continue")]
    public void TargetedTransfersSkipStatementsAndReachDestination(string kind)
    {
        var target = Label("target");
        BoundStatement transfer = kind switch
        {
            "break" => new BoundBreakStatement(target),
            "continue" => new BoundContinueStatement(target),
            _ => new BoundGotoStatement(target),
        };
        var destination = new BoundLabeledStatement(target, Statement());
        var tail = new BoundReturnStatement(null);
        var result = Rewrite(new BoundBlockStatement([transfer, Statement(), destination, tail]));
        Assert.Equal(new BoundStatement[] { transfer, destination, tail }, result.Statements);
    }

    [Fact]
    public void ConditionalJumpRetainsBothSuccessors()
    {
        var target = Label("target");
        var branch = new BoundConditionalGotoStatement(target, Value(), true);
        var fallthrough = Statement();
        var destination = new BoundLabeledStatement(target, Statement());
        var block = new BoundBlockStatement([branch, fallthrough, destination]);
        Assert.Equal(block.Statements, Rewrite(block).Statements);
    }

    [Fact]
    public void BackwardJumpTerminatesAnalysisAndRemovesUnreachableTail()
    {
        var target = Label("loop");
        var start = new BoundLabeledStatement(target, Statement());
        var jump = new BoundGotoStatement(target);
        var result = Rewrite(new BoundBlockStatement([start, jump, Statement()]));
        Assert.Equal(new BoundStatement[] { start, jump }, result.Statements);
    }

    [Fact]
    public void SourceLabelRemainsAnEntryPointAfterReturn()
    {
        var terminal = new BoundReturnStatement(null);
        var target = Label("source");
        Set(target, "get_Locations", ImmutableArray.Create(Location.Create("source.rvn", new TextSpan(0, 1), default)));
        var destination = new BoundLabeledStatement(target, Statement());
        var tail = Statement();
        var result = Rewrite(new BoundBlockStatement([terminal, Statement(), destination, tail]));
        Assert.Equal(new BoundStatement[] { terminal, destination, tail }, result.Statements);
    }

    [Fact]
    public void NestedTransferRetainsOuterTargetAndChildrenAreRewritten()
    {
        var target = Label("outer");
        var jump = new BoundGotoStatement(target);
        var child = new BoundBlockStatement([jump, Statement()]);
        var terminal = new BoundReturnStatement(null);
        var destination = new BoundLabeledStatement(target, Statement());
        var result = Rewrite(new BoundBlockStatement([child, terminal, Statement(), destination]));
        var statements = result.Statements.ToArray();
        Assert.Equal(3, statements.Length);
        Assert.Same(jump, Assert.Single(Assert.IsType<BoundBlockStatement>(statements[0]).Statements));
        Assert.Same(terminal, statements[1]);
        Assert.Same(destination, statements[2]);
    }

    [Fact]
    public void EqualLabelNamesDoNotConflateDistinctTargets()
    {
        var external = Label("same");
        var local = Label("same");
        var jump = new BoundGotoStatement(external);
        var result = Rewrite(new BoundBlockStatement([jump, new BoundLabeledStatement(local, Statement())]));
        Assert.Same(jump, Assert.Single(result.Statements));
    }

    [Fact]
    public void EmptySingleAndFullyReachableBlocksPreserveStatements()
    {
        foreach (var block in new[]
        {
            new BoundBlockStatement([]),
            new BoundBlockStatement([Statement()]),
            new BoundBlockStatement([Statement(), Statement()]),
        })
            Assert.Equal(block.Statements, Rewrite(block).Statements);
    }

    private static LabelSymbol Label(string name)
    {
        var label = Make<LabelSymbol>(SymbolKind.Label, name);
        Set(label, "get_Locations", ImmutableArray<Location>.Empty);
        return label;
    }

    private static BoundExpression Value() => new BoundDefaultValueExpression(Named("Runtime", "Example.Value"));

    private static BoundStatement Statement() => new BoundExpressionStatement(Value());

    private static BoundBlockStatement Rewrite(BoundBlockStatement block)
        => Assert.IsType<BoundBlockStatement>(UnreachableCodeOptimizer.Rewrite(block));
}
