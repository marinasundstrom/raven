namespace Raven.CodeAnalysis;

/// <summary>
/// Simplifies built-in Boolean expressions when a literal operand determines
/// the result without changing which non-literal operands are evaluated.
/// </summary>
internal sealed class BooleanExpressionOptimizer : BoundTreeRewriter
{
    private readonly Compilation _compilation;

    private BooleanExpressionOptimizer(Compilation compilation)
    {
        _compilation = compilation;
    }

    public static BoundNode Rewrite(Compilation compilation, BoundNode node)
        => new BooleanExpressionOptimizer(compilation).Visit(node)!;

    public override BoundNode? VisitBinaryExpression(BoundBinaryExpression node)
    {
        var rewritten = (BoundBinaryExpression)base.VisitBinaryExpression(node)!;
        if (rewritten.Operator.MethodSymbol is not null)
            return rewritten;

        if (TryRewriteBooleanComparison(rewritten, out var comparison))
            return comparison;

        if (BoundBooleanFacts.TryGetConstantValue(rewritten.Left, out var left))
        {
            return rewritten.Operator.OperatorKind switch
            {
                BinaryOperatorKind.LogicalAnd => left ? rewritten.Right : rewritten.Left,
                BinaryOperatorKind.LogicalOr => left ? rewritten.Left : rewritten.Right,
                _ => rewritten,
            };
        }

        if (BoundBooleanFacts.TryGetConstantValue(rewritten.Right, out var right))
        {
            return rewritten.Operator.OperatorKind switch
            {
                BinaryOperatorKind.LogicalAnd when right => rewritten.Left,
                BinaryOperatorKind.LogicalOr when !right => rewritten.Left,
                _ => rewritten,
            };
        }

        return rewritten;
    }

    public override BoundNode? VisitUnaryExpression(BoundUnaryExpression node)
    {
        var rewritten = (BoundUnaryExpression)base.VisitUnaryExpression(node)!;
        if (rewritten.Operator.OperatorKind != BoundUnaryOperatorKind.LogicalNot ||
            !BoundBooleanFacts.TryGetConstantValue(rewritten.Operand, out var operand))
        {
            return rewritten;
        }

        return new BoundLiteralExpression(
            operand ? BoundLiteralExpressionKind.FalseLiteral : BoundLiteralExpressionKind.TrueLiteral,
            !operand,
            rewritten.Type!);
    }

    private bool TryRewriteBooleanComparison(
        BoundBinaryExpression expression,
        out BoundExpression rewritten)
    {
        if (expression.Operator.OperatorKind is not (BinaryOperatorKind.Equality or BinaryOperatorKind.Inequality))
        {
            rewritten = expression;
            return false;
        }

        var leftIsConstant = BoundBooleanFacts.TryGetConstantValue(expression.Left, out var left);
        var rightIsConstant = BoundBooleanFacts.TryGetConstantValue(expression.Right, out var right);
        if (!leftIsConstant && !rightIsConstant)
        {
            rewritten = expression;
            return false;
        }

        if (leftIsConstant && rightIsConstant)
        {
            var result = expression.Operator.OperatorKind == BinaryOperatorKind.Equality
                ? left == right
                : left != right;
            rewritten = CreateBooleanLiteral(result, expression.Type);
            return true;
        }

        var operand = leftIsConstant ? expression.Right : expression.Left;
        var constant = leftIsConstant ? left : right;
        var keepOperand = expression.Operator.OperatorKind == BinaryOperatorKind.Equality
            ? constant
            : !constant;

        if (keepOperand)
        {
            rewritten = operand;
            return true;
        }

        if (!BoundUnaryOperator.TryLookup(
            _compilation,
            Syntax.SyntaxKind.ExclamationToken,
            operand.Type,
            out var logicalNot))
        {
            rewritten = expression;
            return false;
        }

        rewritten = new BoundUnaryExpression(logicalNot, operand);
        return true;
    }

    private static BoundLiteralExpression CreateBooleanLiteral(bool value, ITypeSymbol type)
        => new(
            value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            value,
            type);
}
