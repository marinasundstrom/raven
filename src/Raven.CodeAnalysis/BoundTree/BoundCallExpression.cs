using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

sealed class BoundCallExpression : BoundExpression
{
    public IMethodSymbol Method { get; }
    public BoundExpression[] Arguments { get; }
    public BoundExpression? Receiver { get; }

    public BoundCallExpression(IMethodSymbol method, BoundExpression[] arguments, BoundExpression? receiver = null)
           : base(method.ReturnType, method, BoundExpressionReason.None)
    {
        Method = method;
        Arguments = arguments;
        Receiver = receiver;
    }
}