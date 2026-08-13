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
}
