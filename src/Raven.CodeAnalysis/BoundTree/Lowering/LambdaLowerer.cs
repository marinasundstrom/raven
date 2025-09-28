using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class LambdaLowerer
{
    public static BoundLambdaExpression Rewrite(BoundLambdaExpression lambda, ISymbol containingSymbol)
    {
        var owner = lambda.Symbol ?? containingSymbol;

        var loweredBody = LowerBody(lambda.Body, owner);
        if (ReferenceEquals(loweredBody, lambda.Body))
            return lambda;

        var lowered = new BoundLambdaExpression(
            lambda.Parameters,
            lambda.ReturnType,
            loweredBody,
            owner,
            lambda.DelegateType,
            lambda.CapturedVariables,
            lambda.CandidateDelegates);

        if (lambda.Unbound is { } unbound)
            lowered.AttachUnbound(unbound);

        return lowered;
    }

    private static BoundExpression LowerBody(BoundExpression body, ISymbol owner)
    {
        if (body is BoundBlockExpression block)
        {
            var loweredBlock = Lowerer.LowerBlock(owner, new BoundBlockStatement(block.Statements, block.LocalsToDispose));
            return new BoundBlockExpression(loweredBlock.Statements, block.UnitType, loweredBlock.LocalsToDispose);
        }

        return Lowerer.LowerExpression(owner, body);
    }
}
