namespace Raven.CodeAnalysis;

internal static class BoundExpressionNullabilityExtensions
{
    public static ITypeSymbol GetNullabilityFlowType(this BoundExpression expression)
    {
        switch (expression)
        {
            case BoundInvocationExpression invocation
                when NullableFlowAttributeFacts.ReturnMayBeNull(invocation.Method) &&
                    !invocation.Type.IsValueType:
                return invocation.Type.WithNullableAnnotation(NullableAnnotation.Annotated);

            case BoundParenthesizedExpression parenthesized:
                return PreserveNullableFlow(parenthesized.Type, parenthesized.Expression.GetNullabilityFlowType());

            case BoundRequiredResultExpression requiredResult:
                return PreserveNullableFlow(requiredResult.Type, requiredResult.Operand.GetNullabilityFlowType());

            case BoundConversionExpression { IsNullabilityFlowNarrowing: false } conversion:
                return PreserveNullableFlow(conversion.Type, conversion.Expression.GetNullabilityFlowType());

            default:
                return expression.Type;
        }
    }

    private static ITypeSymbol PreserveNullableFlow(ITypeSymbol declaredType, ITypeSymbol operandFlowType)
        => operandFlowType.IsNullable && !declaredType.IsNullable
            ? declaredType.WithNullableAnnotation(NullableAnnotation.Annotated)
            : declaredType;
}
