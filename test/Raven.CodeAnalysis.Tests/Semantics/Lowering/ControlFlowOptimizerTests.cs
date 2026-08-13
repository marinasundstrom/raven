using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests.Semantics.Lowering;

public sealed class ControlFlowOptimizerTests
{
    [Fact]
    public void Rewrite_SelectsLiteralIfBranches()
    {
        var compilation = Compilation.Create(
            "control_flow_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);
        var whenTrue = new BoundUnitExpression(unitType);
        var whenFalse = new BoundUnitExpression(unitType);

        var trueCondition = new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType);
        var falseCondition = new BoundLiteralExpression(
            BoundLiteralExpressionKind.FalseLiteral,
            false,
            booleanType);

        Assert.Same(
            whenTrue,
            ControlFlowOptimizer.Rewrite(new BoundIfExpression(trueCondition, whenTrue, whenFalse)));
        Assert.Same(
            whenFalse,
            ControlFlowOptimizer.Rewrite(new BoundIfExpression(falseCondition, whenTrue, whenFalse)));

        var trueStatement = new BoundExpressionStatement(whenTrue);
        var falseStatement = new BoundExpressionStatement(whenFalse);

        Assert.Same(
            trueStatement,
            ControlFlowOptimizer.Rewrite(new BoundIfStatement(trueCondition, trueStatement, falseStatement)));
        Assert.Same(
            falseStatement,
            ControlFlowOptimizer.Rewrite(new BoundIfStatement(falseCondition, trueStatement, falseStatement)));
    }

    [Theory]
    [InlineData(true, true, true)]
    [InlineData(true, false, false)]
    [InlineData(false, true, false)]
    [InlineData(false, false, true)]
    public void Rewrite_SelectsLiteralConditionalGoto(
        bool conditionValue,
        bool jumpIfTrue,
        bool shouldJump)
    {
        var compilation = Compilation.Create(
            "control_flow_optimizer",
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
        var condition = new BoundLiteralExpression(
            conditionValue ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            conditionValue,
            booleanType);

        var rewritten = ControlFlowOptimizer.Rewrite(
            new BoundConditionalGotoStatement(label, condition, jumpIfTrue));

        if (shouldJump)
            Assert.Same(label, Assert.IsType<BoundGotoStatement>(rewritten).Target);
        else
            Assert.Empty(Assert.IsType<BoundBlockStatement>(rewritten).Statements);
    }

    [Fact]
    public void Rewrite_RecognizesLiteralThroughIdentityWrappers()
    {
        var compilation = Compilation.Create(
            "control_flow_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var booleanType = compilation.GetSpecialType(SpecialType.System_Boolean);
        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.TrueLiteral,
            true,
            booleanType);
        var wrapped = new BoundParenthesizedExpression(new BoundConversionExpression(
            literal,
            booleanType,
            new Conversion(isImplicit: true, isIdentity: true)));
        var whenTrue = new BoundExpressionStatement(new BoundUnitExpression(
            compilation.GetSpecialType(SpecialType.System_Unit)));

        var rewritten = ControlFlowOptimizer.Rewrite(new BoundIfStatement(wrapped, whenTrue));

        Assert.IsType<BoundExpressionStatement>(rewritten);
    }
}
