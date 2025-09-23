using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

static class BoundExpressionExtensions
{
    public static SymbolInfo GetSymbolInfo(this BoundExpression expression)
    {
        if (expression is BoundDelegateCreationExpression delegateCreation)
        {
            var group = delegateCreation.MethodGroup;
            var candidates = ImmutableArray.CreateRange(group.Methods, static method => (ISymbol)method);

            if (delegateCreation.Reason is BoundExpressionReason.None)
                return new SymbolInfo(delegateCreation.Method, candidates);

            return new SymbolInfo(Convert(delegateCreation.Reason), candidates);
        }

        if (expression is BoundMethodGroupExpression methodGroup)
        {
            var candidates = ImmutableArray.CreateRange(methodGroup.Methods, static method => (ISymbol)method);

            if (methodGroup.Reason is BoundExpressionReason.None)
                return new SymbolInfo(methodGroup.SelectedMethod, candidates, CandidateReason.MemberGroup);

            return new SymbolInfo(Convert(methodGroup.Reason), candidates);
        }

        if (expression.Reason is BoundExpressionReason.None)
            return new SymbolInfo(expression.Symbol!);

        return new SymbolInfo(Convert(expression.Reason), expression.Symbol is not null ? [expression.Symbol] : []);
    }

    public static SymbolInfo GetSymbolInfo(this BoundStatement statement)
    {
        return new SymbolInfo(statement.Symbol);
    }

    internal static CandidateReason Convert(BoundExpressionReason reason)
    {
        return reason switch
        {
            BoundExpressionReason.None => CandidateReason.None,
            BoundExpressionReason.NotFound => CandidateReason.NotFound,
            BoundExpressionReason.Ambiguous => CandidateReason.Ambiguous,
            BoundExpressionReason.Inaccessible => CandidateReason.Inaccessible,
            BoundExpressionReason.WrongArity => CandidateReason.WrongArity,
            BoundExpressionReason.OverloadResolutionFailed => CandidateReason.OverloadResolutionFailure,
            BoundExpressionReason.TypeMismatch => CandidateReason.None,
            BoundExpressionReason.MissingType => CandidateReason.NotFound,
            BoundExpressionReason.ConstantExpected => CandidateReason.None,
            BoundExpressionReason.UnsupportedOperation => CandidateReason.None,
            BoundExpressionReason.OtherError => CandidateReason.None,
            BoundExpressionReason.ArgumentBindingFailed => CandidateReason.OverloadResolutionFailure,
            _ => throw new ArgumentOutOfRangeException(nameof(reason), reason, null)
        };
    }
}
