using Raven.CodeAnalysis;
using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BooleanExpressionOptimizerContractTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Rewrite_FoldsLiteralLogicalNot(bool value)
    {
        var (compilation, booleanType) = CreateCompilation();
        Assert.True(BoundUnaryOperator.TryLookup(
            compilation,
            SyntaxKind.ExclamationToken,
            booleanType,
            out var logicalNot));
        var literal = BooleanLiteral(value, booleanType);

        var rewritten = Assert.IsType<BoundLiteralExpression>(
            BooleanExpressionOptimizer.Rewrite(compilation, new BoundUnaryExpression(logicalNot, literal)));

        Assert.Equal(value ? BoundLiteralExpressionKind.FalseLiteral : BoundLiteralExpressionKind.TrueLiteral, rewritten.Kind);
        Assert.Equal(!value, rewritten.Value);
    }

    [Theory]
    [InlineData(SyntaxKind.AmpersandAmpersandToken, false)]
    [InlineData(SyntaxKind.AmpersandAmpersandToken, true)]
    [InlineData(SyntaxKind.BarBarToken, true)]
    [InlineData(SyntaxKind.BarBarToken, false)]
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
        var right = CreateParameterAccess(compilation, booleanType);

        var rewritten = BooleanExpressionOptimizer.Rewrite(
            compilation,
            new BoundBinaryExpression(literal, logicalOperator, right));

        var shortCircuits = operatorKind == SyntaxKind.AmpersandAmpersandToken ? !value : value;
        Assert.Same(shortCircuits ? (BoundExpression)literal : right, rewritten);
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
    [InlineData(SyntaxKind.EqualsEqualsToken, true, true, true)]
    [InlineData(SyntaxKind.EqualsEqualsToken, false, false, true)]
    [InlineData(SyntaxKind.EqualsEqualsToken, false, true, false)]
    [InlineData(SyntaxKind.NotEqualsToken, true, false, true)]
    [InlineData(SyntaxKind.NotEqualsToken, true, true, false)]
    [InlineData(SyntaxKind.NotEqualsToken, false, false, false)]
    [InlineData(SyntaxKind.NotEqualsToken, false, true, true)]
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

    [Fact]
    public void RecursiveRewritingFoldsChildrenBeforeParents()
    {
        var (provider, type) = CreateCompilation();
        Assert.True(BoundUnaryOperator.TryLookup(provider, SyntaxKind.ExclamationToken, type, out var op));
        var nested = new BoundUnaryExpression(op, new BoundUnaryExpression(op, BooleanLiteral(true, type)));
        Assert.Equal(true, Assert.IsType<BoundLiteralExpression>(BooleanExpressionOptimizer.Rewrite(provider, nested)).Value);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void ConversionIdentityDeterminesWhetherLiteralCanFold(bool identity)
    {
        var (provider, type) = CreateCompilation();
        Assert.True(BoundUnaryOperator.TryLookup(provider, SyntaxKind.ExclamationToken, type, out var op));
        var operand = new BoundConversionExpression(BooleanLiteral(true, type), type,
            new Conversion(isImplicit: true, isIdentity: identity));
        var expression = new BoundUnaryExpression(op, operand);
        var rewritten = BooleanExpressionOptimizer.Rewrite(provider, expression);
        if (identity)
            Assert.Equal(false, Assert.IsType<BoundLiteralExpression>(rewritten).Value);
        else
            Assert.Same(expression, rewritten);
    }

    private static (Provider Compilation, TypeSymbol BooleanType) CreateCompilation()
    {
        var provider = new Provider();
        return (provider, provider.GetSpecialType(SpecialType.System_Boolean));
    }

    private static BoundLiteralExpression BooleanLiteral(bool value, TypeSymbol booleanType)
        => new(value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            value, booleanType);

    private static BoundParameterAccess CreateParameterAccess(Provider provider, TypeSymbol booleanType)
    {
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Set(parameter, "get_Type", booleanType);
        return new BoundParameterAccess(parameter);
    }

    private sealed class Provider : BinaryOperatorServices
    {
        private readonly Dictionary<SpecialType, NamedTypeSymbol> _types = new();

        public TypeSymbol ErrorTypeSymbol { get; } = Make<TypeSymbol>(SymbolKind.Type, "error", TypeKind.Error);

        public bool HasRuntimeSelfContract => false;

        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method) => null;

        public TypeSymbol CreateNullableType(TypeSymbol type) => throw new NotSupportedException();

        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
            => new(isIdentity: ReferenceEquals(source, destination));

        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            if (!_types.TryGetValue(specialType, out var type))
            {
                type = Named("Runtime", specialType.ToString());
                Set(type, "get_SpecialType", specialType);
                Set(type, "get_EnumUnderlyingType", null);
                _types.Add(specialType, type);
            }
            return type;
        }
    }
}
