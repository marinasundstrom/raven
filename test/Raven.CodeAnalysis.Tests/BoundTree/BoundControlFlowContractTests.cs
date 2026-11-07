using System.Collections.Immutable;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundControlFlowContractTests
{
    [Fact]
    public void BlocksSnapshotStatementsAndNormalizeOptionalCollections()
    {
        var first = new BoundReturnStatement(null);
        var second = new BoundBreakStatement();
        BoundStatement[] input = [first];
        var block = new BoundBlockStatement(input);
        input[0] = second;
        Assert.Same(first, Assert.Single(block.Statements));
        Assert.True(block.LocalsToDispose.IsEmpty);
        Assert.False(block.LocalsToDispose.IsDefault);
        Assert.True(block.IntroduceILScope);
        Assert.Empty(new BoundBlockStatement(null).Statements);
        var enumerations = 0;
        IEnumerable<BoundStatement> Statements()
        {
            enumerations++;
            yield return first;
        }
        var snapshot = new BoundBlockStatement(Statements(), introduceILScope: false);
        Assert.Equal(1, enumerations);
        Assert.Same(first, Assert.Single(snapshot.Statements));
        Assert.Same(first, Assert.Single(snapshot.Statements));
        Assert.Equal(1, enumerations);
        Assert.False(snapshot.IntroduceILScope);
        Assert.Same(block, block.Update(block.Statements, block.LocalsToDispose, true));
        var local = Make<LocalSymbol>(SymbolKind.Local, "resource");
        var changed = block.Update(block.Statements, ImmutableArray.Create(local), false);
        Assert.Same(local, Assert.Single(changed.LocalsToDispose));
        Assert.True(block.LocalsToDispose.IsEmpty);
        Assert.False(changed.IntroduceILScope);
    }

    [Fact]
    public void WalkerVisitsConditionsAndBothBranchesInSourceOrder()
    {
        var type = Named("A", "Example.Value");
        BoundExpression Literal(int value) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, type);
        var label = Make<LabelSymbol>(SymbolKind.Label, "end");
        var tree = new BoundBlockStatement([
            new BoundIfStatement(Literal(1), new BoundReturnStatement(Literal(2)), new BoundThrowStatement(Literal(3))),
            new BoundWhileStatement(Literal(4), new BoundLoopStatement(new BoundExpressionStatement(Literal(5)))),
            new BoundConditionalGotoStatement(label, Literal(6), true),
            new BoundLabeledStatement(label, new BoundExpressionStatement(Literal(7))),
            new BoundReturnStatement(null),
            new BoundIfStatement(Literal(8), new BoundContinueStatement()),
            new BoundBreakStatement(), new BoundGotoStatement(label)
        ]);
        var walker = new RecordingWalker();
        walker.Visit(tree);
        Assert.Equal([1, 2, 3, 4, 5, 6, 7, 8], walker.Values);
    }

    [Fact]
    public void ExitNodesPreserveLabelsDirectionAndFailureIntent()
    {
        var label = Make<LabelSymbol>(SymbolKind.Label, "target");
        var type = Named("A", "Example.Value");
        var expression = new BoundDefaultValueExpression(type);
        var jump = new BoundGotoStatement(label, true);
        Assert.Same(label, jump.Symbol);
        Assert.True(jump.IsBackward);
        Assert.Same(jump, jump.Update(label, true));
        Assert.False(jump.Update(label, false).IsBackward);
        var conditional = new BoundConditionalGotoStatement(label, expression, false);
        Assert.Same(label, conditional.Symbol);
        Assert.False(conditional.JumpIfTrue);
        Assert.Same(conditional, conditional.Update(label, expression, false));
        var labeled = new BoundLabeledStatement(label, jump);
        Assert.Same(label, labeled.Symbol);
        Assert.Same(labeled, labeled.Update(label, jump));
        Assert.Same(label, new BoundContinueStatement(label).TargetLabel);
        Assert.Same(label, new BoundBreakStatement(label).TargetLabel);
        Assert.Null(new BoundBreakStatement().TargetLabel);
        var failure = new BoundThrowStatement(expression, "unreachable match arm");
        Assert.Equal("unreachable match arm", failure.CompilerFailure);
        Assert.Null(new BoundThrowStatement(expression).CompilerFailure);
        Assert.Same(failure, failure.Update(expression, failure.CompilerFailure));
    }

    private sealed class RecordingWalker : BoundTreeWalker
    {
        internal List<int> Values { get; } = [];
        public override void VisitLiteralExpression(BoundLiteralExpression node) => Values.Add((int)node.Value);
    }
}
