namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitBinaryExpression(BoundBinaryExpression node)
    {
        var rewritten = (BoundBinaryExpression)base.VisitBinaryExpression(node)!;
        // Binding already diagnosed invalid source constants. Materialize valid
        // values here so constant division/remainder do not acquire runtime faults.
        var status = BoundNumericConstantEvaluator.Evaluate(rewritten, expression => expression switch
        {
            BoundBinaryExpression binary => (binary.Operator.OperatorKind & BinaryOperatorKind.Checked) != 0,
            BoundUnaryExpression unary => unary.Operator.OperatorKind == BoundUnaryOperatorKind.CheckedUnaryMinus,
            BoundConversionExpression conversion => conversion.IsChecked,
            _ => false,
        }, out var value);
        return status == BoundNumericConstantEvaluator.Status.Constant
            ? new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value!, rewritten.Type)
            : rewritten;
    }
}
