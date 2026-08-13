using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Semantics.Lowering;

public sealed class BranchOptimizerTests
{
    [Fact]
    public void Rewrite_RemovesGotoToImmediatelyFollowingLabel()
    {
        var compilation = Compilation.Create(
            "branch_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var label = new LabelSymbol(
            "end",
            compilation.Module,
            containingType: null,
            containingNamespace: compilation.GlobalNamespace,
            locations: [],
            declaringSyntaxReferences: []);
        var labeled = new BoundLabeledStatement(label, new BoundBlockStatement([]));
        var block = new BoundBlockStatement([new BoundGotoStatement(label), labeled]);

        var rewritten = Assert.IsType<BoundBlockStatement>(BranchOptimizer.Rewrite(block));

        var rewrittenLabel = Assert.IsType<BoundLabeledStatement>(Assert.Single(rewritten.Statements));
        Assert.Same(label, rewrittenLabel.Label);
    }

    [Fact]
    public void Rewrite_KeepsGotoWhenTargetIsNotImmediatelyFollowing()
    {
        var compilation = Compilation.Create(
            "branch_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var label = new LabelSymbol(
            "end",
            compilation.Module,
            containingType: null,
            containingNamespace: compilation.GlobalNamespace,
            locations: [],
            declaringSyntaxReferences: []);
        var @goto = new BoundGotoStatement(label);
        var intervening = new BoundExpressionStatement(new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType));
        var labeled = new BoundLabeledStatement(label, new BoundBlockStatement([]));
        var block = new BoundBlockStatement([@goto, intervening, labeled]);

        var rewritten = Assert.IsType<BoundBlockStatement>(BranchOptimizer.Rewrite(block));

        Assert.IsType<BoundGotoStatement>(rewritten.Statements.First());
        Assert.Equal(3, rewritten.Statements.Count());
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Rewrite_InvertsConditionalBranchOverGoto(bool jumpIfTrue)
    {
        var compilation = Compilation.Create(
            "branch_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var fallthroughLabel = CreateLabel(compilation, "fallthrough");
        var targetLabel = CreateLabel(compilation, "target");
        var condition = new BoundParameterAccess(new SourceParameterSymbol(
            "condition",
            booleanType,
            compilation.Module,
            containingType: null,
            containingNamespace: compilation.GlobalNamespace,
            locations: [],
            declaringSyntaxReferences: []));
        var fallthrough = new BoundLabeledStatement(
            fallthroughLabel,
            new BoundBlockStatement([]));
        var block = new BoundBlockStatement([
            new BoundConditionalGotoStatement(fallthroughLabel, condition, jumpIfTrue),
            new BoundGotoStatement(targetLabel),
            fallthrough,
        ]);

        var rewritten = Assert.IsType<BoundBlockStatement>(BranchOptimizer.Rewrite(block));

        Assert.Collection(
            rewritten.Statements,
            statement =>
            {
                var conditional = Assert.IsType<BoundConditionalGotoStatement>(statement);
                Assert.Same(targetLabel, conditional.Target);
                Assert.Equal(!jumpIfTrue, conditional.JumpIfTrue);
                Assert.IsType<BoundParameterAccess>(conditional.Condition);
            },
            statement => Assert.Same(fallthroughLabel, Assert.IsType<BoundLabeledStatement>(statement).Label));
    }

    private static LabelSymbol CreateLabel(Compilation compilation, string name)
        => new(
            name,
            compilation.Module,
            containingType: null,
            containingNamespace: compilation.GlobalNamespace,
            locations: [],
            declaringSyntaxReferences: []);
}
