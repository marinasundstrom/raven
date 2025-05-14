using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

class BoundCallExpression : BoundExpression
{
    public IMethodSymbol Method { get; }
    public IReadOnlyList<BoundExpression> Arguments { get; }

    public BoundCallExpression(IMethodSymbol method, IReadOnlyList<BoundExpression> arguments)
        : base(method.ReturnType, method, CandidateReason.None)
    {
        Method = method;
        Arguments = arguments;
    }
}