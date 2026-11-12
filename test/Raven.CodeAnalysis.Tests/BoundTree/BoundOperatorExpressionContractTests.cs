using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.BoundTree;

[Trait("Target", "Shared")]
public sealed class BoundOperatorExpressionContractTests
{
    [Fact]
    public void BinaryExpressionPreservesLookupAndOperands()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, integer, integer, out var op));
        var left = Literal(integer, 1);
        var right = Literal(integer, 2);
        var node = new BoundBinaryExpression(left, op, right);
        Assert.Same(left, node.Left);
        Assert.Same(right, node.Right);
        Assert.Same(op, node.Operator);
        Assert.Same(op.ResultType, node.Type);
        Assert.Same(op.MethodSymbol, node.Symbol);
        Assert.Equal(BoundExpressionReason.None, node.Reason);
    }

    [Fact]
    public void UnaryExpressionUsesPromotedResultType()
    {
        var services = new Services();
        var small = services.GetSpecialType(SpecialType.System_Byte);
        Assert.True(BoundUnaryOperator.TryLookup(services, SyntaxKind.MinusToken, small, out var op));
        var operand = Literal(small, 1);
        var node = new BoundUnaryExpression(op, operand);
        Assert.Same(operand, node.Operand);
        Assert.Same(op, node.Operator);
        Assert.Same(services.GetSpecialType(SpecialType.System_Int32), node.Type);
        Assert.Null(node.Symbol);
        Assert.Equal(BoundExpressionReason.None, node.Reason);
    }

    [Fact]
    public void WalkerVisitsNestedOperandsInSourceOrderExactlyOnce()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, integer, integer, out var binary));
        Assert.True(BoundUnaryOperator.TryLookup(services, SyntaxKind.MinusToken, integer, out var unary));
        var left = Literal(integer, 1);
        var right = Literal(integer, 2);
        var negation = new BoundUnaryExpression(unary, right);
        var expression = new BoundBinaryExpression(left, binary, negation);
        var walker = new RecordingWalker();
        walker.Visit(expression);
        Assert.Equal<BoundExpression>([expression, left, negation, right], walker.Nodes);
    }

    [Fact]
    public void UpdatesReuseUnchangedNodesAndPreserveLookupOnReplacement()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, integer, integer, out var binary));
        Assert.True(BoundUnaryOperator.TryLookup(services, SyntaxKind.MinusToken, integer, out var unary));
        var first = Literal(integer, 1);
        var second = Literal(integer, 2);
        var negation = new BoundUnaryExpression(unary, first);
        Assert.Same(negation, negation.Update(unary, first));
        var changedUnary = negation.Update(unary, second);
        Assert.NotSame(negation, changedUnary);
        Assert.Same(second, changedUnary.Operand);
        Assert.Same(unary.ResultType, changedUnary.Type);
        var expression = new BoundBinaryExpression(first, binary, negation);
        Assert.Same(expression, expression.Update(first, binary, negation));
        var changedBinary = expression.Update(second, binary, changedUnary);
        Assert.NotSame(expression, changedBinary);
        Assert.Same(second, changedBinary.Left);
        Assert.Same(changedUnary, changedBinary.Right);
        Assert.Same(binary, changedBinary.Operator);
        Assert.Same(binary.ResultType, changedBinary.Type);
        Assert.Same(binary.MethodSymbol, changedBinary.Symbol);
    }

    [Fact]
    public void GenericVisitorDispatchesToEachOperatorNode()
    {
        var services = new Services();
        var integer = services.GetSpecialType(SpecialType.System_Int32);
        Assert.True(BoundBinaryOperator.TryLookup(services, SyntaxKind.PlusToken, integer, integer, out var binary));
        Assert.True(BoundUnaryOperator.TryLookup(services, SyntaxKind.MinusToken, integer, out var unary));
        var operand = Literal(integer, 1);
        var visitor = new OperatorVisitor();
        Assert.Equal("binary", new BoundBinaryExpression(operand, binary, operand).Accept(visitor));
        Assert.Equal("unary", new BoundUnaryExpression(unary, operand).Accept(visitor));
    }

    private sealed class OperatorVisitor : BoundTreeVisitor<string>
    {
        public override string VisitBinaryExpression(BoundBinaryExpression node) => "binary";
        public override string VisitUnaryExpression(BoundUnaryExpression node) => "unary";
    }

    private static BoundLiteralExpression Literal(TypeSymbol type, int value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, type);

    private sealed class RecordingWalker : BoundTreeWalker
    {
        public List<BoundExpression> Nodes { get; } = [];

        public override void VisitExpression(BoundExpression node)
        {
            Nodes.Add(node);
            base.VisitExpression(node);
        }
    }

    private sealed class Services : BinaryOperatorServices
    {
        private readonly Dictionary<SpecialType, NamedTypeSymbol> _types = [];

        public TypeSymbol ErrorTypeSymbol => throw new InvalidOperationException("Unexpected error recovery.");
        public bool HasRuntimeSelfContract => false;

        public NamedTypeSymbol GetSpecialType(SpecialType type)
        {
            if (_types.TryGetValue(type, out var result))
                return result;
            result = Make<NamedTypeSymbol>(SymbolKind.Type, type.ToString(), TypeKind.Struct);
            Set(result, "get_SpecialType", type);
            _types.Add(type, result);
            return result;
        }

        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
            => throw new InvalidOperationException("Unexpected conversion.");

        public TypeSymbol CreateNullableType(TypeSymbol type)
            => throw new InvalidOperationException("Unexpected nullable lifting.");

        public MethodSymbol? ProjectRuntimeSelfOperator(TypeSymbol receiver, MethodSymbol method)
            => throw new InvalidOperationException("Unexpected Self projection.");
    }
}
