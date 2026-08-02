namespace Raven.CodeAnalysis;

internal static class BoundExpressionNullabilityExtensions
{
    public static ITypeSymbol GetNullabilityFlowType(this BoundExpression expression)
    {
        switch (expression)
        {
            case BoundNullabilityFlowExpression flowExpression:
                return flowExpression.FlowType;

            case BoundInvocationExpression invocation
                when InvocationReturnIsNotNull(invocation):
                return invocation.Type.WithNullableAnnotation(NullableAnnotation.NotAnnotated);

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

    private static bool InvocationReturnIsNotNull(BoundInvocationExpression invocation)
    {
        if (!NullableFlowAttributeFacts.TryGetNotNullIfNotNull(invocation.Method, out var parameterName))
            return false;

        var parameterIndex = -1;
        for (var i = 0; i < invocation.Method.Parameters.Length; i++)
        {
            if (string.Equals(invocation.Method.Parameters[i].Name, parameterName, StringComparison.Ordinal))
            {
                parameterIndex = i;
                break;
            }
        }

        if (parameterIndex < 0)
            return false;

        BoundExpression? argument;
        if (invocation.Method.IsExtensionMethod && invocation.ExtensionReceiver is not null)
        {
            if (parameterIndex == 0)
                argument = invocation.ExtensionReceiver;
            else
                argument = invocation.Arguments.ElementAtOrDefault(parameterIndex - 1);
        }
        else
        {
            argument = invocation.Arguments.ElementAtOrDefault(parameterIndex);
        }

        return argument is not null && !argument.GetNullabilityFlowType().IsNullable;
    }

    private static ITypeSymbol PreserveNullableFlow(ITypeSymbol declaredType, ITypeSymbol operandFlowType)
        => operandFlowType.IsNullable && !declaredType.IsNullable
            ? declaredType.WithNullableAnnotation(NullableAnnotation.Annotated)
            : declaredType;
}
