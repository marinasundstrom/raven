using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Semantics.Lowering;

public sealed class UnreachableCodeOptimizerTests
{
    [Fact]
    public void Rewrite_RemovesStatementAfterReturn()
    {
        var compilation = Compilation.Create(
            "unreachable_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var returnStatement = new BoundReturnStatement(expression: null);
        var unreachable = new BoundExpressionStatement(new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType));
        var block = new BoundBlockStatement([returnStatement, unreachable]);

        var rewritten = Assert.IsType<BoundBlockStatement>(UnreachableCodeOptimizer.Rewrite(block));

        Assert.Equal([returnStatement], rewritten.Statements);
    }

    [Fact]
    public void Rewrite_FollowsGotoAndRemovesSkippedStatement()
    {
        var compilation = Compilation.Create(
            "unreachable_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var label = new LabelSymbol(
            "target",
            compilation.Module,
            containingType: null,
            containingNamespace: compilation.GlobalNamespace,
            locations: [],
            declaringSyntaxReferences: []);
        var @goto = new BoundGotoStatement(label);
        var skipped = new BoundExpressionStatement(new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType));
        var labeled = new BoundLabeledStatement(label, new BoundBlockStatement([]));
        var afterLabel = new BoundReturnStatement(expression: null);
        var block = new BoundBlockStatement([@goto, skipped, labeled, afterLabel]);

        var rewritten = Assert.IsType<BoundBlockStatement>(UnreachableCodeOptimizer.Rewrite(block));

        Assert.Collection(
            rewritten.Statements,
            statement => Assert.IsType<BoundGotoStatement>(statement),
            statement => Assert.IsType<BoundLabeledStatement>(statement),
            statement => Assert.IsType<BoundReturnStatement>(statement));
    }
}
