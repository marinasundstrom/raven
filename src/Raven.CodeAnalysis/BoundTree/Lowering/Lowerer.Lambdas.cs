using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitLambdaExpression(BoundLambdaExpression node)
    {
        var lowered = LowerLambda(node);
        return lowered;
    }

    private BoundLambdaExpression LowerLambda(BoundLambdaExpression lambda)
    {
        var containingSymbol = lambda.Symbol ?? _containingSymbol;

        var loweredBody = LowerLambdaBody(lambda.Body, containingSymbol);

        if (ReferenceEquals(loweredBody, lambda.Body))
            return lambda;

        var lowered = new BoundLambdaExpression(
            lambda.Parameters,
            lambda.ReturnType,
            loweredBody,
            lambda.Symbol ?? containingSymbol,
            lambda.DelegateType,
            lambda.CapturedVariables,
            lambda.CandidateDelegates);

        if (lambda.Unbound is { } unbound)
            lowered.AttachUnbound(unbound);

        return lowered;
    }

    private BoundExpression LowerLambdaBody(BoundExpression body, ISymbol containingSymbol)
    {
        if (body is BoundBlockExpression block)
        {
            var loweredBlock = LowerBlock(containingSymbol, new BoundBlockStatement(block.Statements, block.LocalsToDispose));
            return new BoundBlockExpression(loweredBlock.Statements, block.UnitType, loweredBlock.LocalsToDispose);
        }

        var lambdaLowerer = new Lowerer(containingSymbol);
        return (BoundExpression)lambdaLowerer.VisitExpression(body)!;
    }
}
