using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Lowering;

public sealed class BooleanExpressionOptimizerTests
{
    [Fact]
    public void Pipeline_OnlySimplifiesBooleanExpressionsInReleaseMode()
    {
        var debugCompilation = CreateCompilation(OptimizationLevel.Debug);
        var debugExpression = CreateFalseAndParameter(debugCompilation.Compilation, debugCompilation.BooleanType);

        Assert.Same(
            debugExpression,
            BoundTreeOptimizer.Optimize<BoundExpression>(debugCompilation.Compilation.Module, debugExpression));

        var releaseCompilation = CreateCompilation(OptimizationLevel.Release);
        var releaseExpression = CreateFalseAndParameter(releaseCompilation.Compilation, releaseCompilation.BooleanType);
        var rewritten = Assert.IsType<BoundLiteralExpression>(
            BoundTreeOptimizer.Optimize<BoundExpression>(releaseCompilation.Compilation.Module, releaseExpression));

        Assert.Equal(false, rewritten.Value);
    }

    [Fact]
    public void Rewrite_FoldsLiteralLogicalNot()
    {
        var (compilation, booleanType) = CreateCompilation();
        Assert.True(BoundUnaryOperator.TryLookup(
            compilation,
            SyntaxKind.ExclamationToken,
            booleanType,
            out var logicalNot));
        var literal = BooleanLiteral(value: true, booleanType);

        var rewritten = Assert.IsType<BoundLiteralExpression>(
            BooleanExpressionOptimizer.Rewrite(compilation, new BoundUnaryExpression(logicalNot, literal)));

        Assert.Equal(BoundLiteralExpressionKind.FalseLiteral, rewritten.Kind);
        Assert.Equal(false, rewritten.Value);
    }

    [Theory]
    [InlineData(SyntaxKind.AmpersandAmpersandToken, false)]
    [InlineData(SyntaxKind.BarBarToken, true)]
    public void Rewrite_FoldsShortCircuitingLeftLiteral(SyntaxKind operatorKind, bool value)
    {
        var (compilation, booleanType) = CreateCompilation();
        Assert.True(BoundBinaryOperator.TryLookup(
            compilation,
            operatorKind,
            booleanType,
            booleanType,
            out var logicalOperator));
        var literal = BooleanLiteral(value, booleanType);
        var right = new BoundParameterAccess(
            new SourceParameterSymbol(
                "right",
                booleanType,
                compilation.Module,
                containingType: null,
                containingNamespace: compilation.GlobalNamespace,
                locations: [],
                declaringSyntaxReferences: [],
                refKind: RefKind.None));

        var rewritten = BooleanExpressionOptimizer.Rewrite(
            compilation,
            new BoundBinaryExpression(literal, logicalOperator, right));

        var rewrittenLiteral = Assert.IsType<BoundLiteralExpression>(rewritten);
        Assert.Equal(value, rewrittenLiteral.Value);
    }

    [Theory]
    [InlineData(SyntaxKind.AmpersandAmpersandToken, true, true)]
    [InlineData(SyntaxKind.BarBarToken, false, true)]
    [InlineData(SyntaxKind.AmpersandAmpersandToken, false, false)]
    [InlineData(SyntaxKind.BarBarToken, true, false)]
    public void Rewrite_PreservesLeftEvaluationForRightLiteral(
        SyntaxKind operatorKind,
        bool rightValue,
        bool canRemoveOperator)
    {
        var (compilation, booleanType) = CreateCompilation();
        Assert.True(BoundBinaryOperator.TryLookup(
            compilation,
            operatorKind,
            booleanType,
            booleanType,
            out var logicalOperator));
        var left = CreateParameterAccess(compilation, booleanType);
        var expression = new BoundBinaryExpression(
            left,
            logicalOperator,
            BooleanLiteral(rightValue, booleanType));

        var rewritten = BooleanExpressionOptimizer.Rewrite(compilation, expression);

        if (canRemoveOperator)
            Assert.IsType<BoundParameterAccess>(rewritten);
        else
            Assert.IsType<BoundBinaryExpression>(rewritten);
    }

    [Theory]
    [InlineData(SyntaxKind.EqualsEqualsToken, true, true, false)]
    [InlineData(SyntaxKind.EqualsEqualsToken, false, true, false)]
    [InlineData(SyntaxKind.NotEqualsToken, true, false, false)]
    [InlineData(SyntaxKind.NotEqualsToken, false, false, false)]
    [InlineData(SyntaxKind.EqualsEqualsToken, true, false, true)]
    [InlineData(SyntaxKind.EqualsEqualsToken, false, false, true)]
    [InlineData(SyntaxKind.NotEqualsToken, true, true, true)]
    [InlineData(SyntaxKind.NotEqualsToken, false, true, true)]
    public void Rewrite_NormalizesBooleanComparisonWithLiteral(
        SyntaxKind operatorKind,
        bool literalOnLeft,
        bool literalValue,
        bool expectedNegation)
    {
        var (compilation, booleanType) = CreateCompilation();
        Assert.True(BoundBinaryOperator.TryLookup(
            compilation,
            operatorKind,
            booleanType,
            booleanType,
            out var comparison));
        var parameter = CreateParameterAccess(compilation, booleanType);
        var literal = BooleanLiteral(literalValue, booleanType);
        var expression = literalOnLeft
            ? new BoundBinaryExpression(literal, comparison, parameter)
            : new BoundBinaryExpression(parameter, comparison, literal);

        var rewritten = BooleanExpressionOptimizer.Rewrite(compilation, expression);

        if (expectedNegation)
        {
            var logicalNot = Assert.IsType<BoundUnaryExpression>(rewritten);
            Assert.Equal(BoundUnaryOperatorKind.LogicalNot, logicalNot.Operator.OperatorKind);
            Assert.IsType<BoundParameterAccess>(logicalNot.Operand);
        }
        else
        {
            Assert.IsType<BoundParameterAccess>(rewritten);
        }
    }

    [Theory]
    [InlineData(SyntaxKind.EqualsEqualsToken, true, false, false)]
    [InlineData(SyntaxKind.NotEqualsToken, true, false, true)]
    public void Rewrite_FoldsBooleanLiteralComparison(
        SyntaxKind operatorKind,
        bool leftValue,
        bool rightValue,
        bool expected)
    {
        var (compilation, booleanType) = CreateCompilation();
        Assert.True(BoundBinaryOperator.TryLookup(
            compilation,
            operatorKind,
            booleanType,
            booleanType,
            out var comparison));
        var expression = new BoundBinaryExpression(
            BooleanLiteral(leftValue, booleanType),
            comparison,
            BooleanLiteral(rightValue, booleanType));

        var rewritten = Assert.IsType<BoundLiteralExpression>(
            BooleanExpressionOptimizer.Rewrite(compilation, expression));

        Assert.Equal(expected, rewritten.Value);
    }

    private static (Compilation Compilation, ITypeSymbol BooleanType) CreateCompilation(
        OptimizationLevel optimizationLevel = OptimizationLevel.Debug)
    {
        var compilation = Compilation.Create(
            "boolean_optimizer",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithOptimizationLevel(optimizationLevel))
            .AddReferences(TestMetadataReferences.Default);
        return (compilation, compilation.GetSpecialType(SpecialType.System_Boolean));
    }

    private static BoundBinaryExpression CreateFalseAndParameter(
        Compilation compilation,
        ITypeSymbol booleanType)
    {
        Assert.True(BoundBinaryOperator.TryLookup(
            compilation,
            SyntaxKind.AmpersandAmpersandToken,
            booleanType,
            booleanType,
            out var logicalAnd));

        return new BoundBinaryExpression(
            BooleanLiteral(value: false, booleanType),
            logicalAnd,
            CreateParameterAccess(compilation, booleanType));
    }

    private static BoundLiteralExpression BooleanLiteral(bool value, ITypeSymbol booleanType)
        => new(
            value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            value,
            booleanType);

    private static BoundParameterAccess CreateParameterAccess(
        Compilation compilation,
        ITypeSymbol booleanType)
        => new(new SourceParameterSymbol(
            "value",
            booleanType,
            compilation.Module,
            containingType: null,
            containingNamespace: compilation.GlobalNamespace,
            locations: [],
            declaringSyntaxReferences: [],
            refKind: RefKind.None));
}
