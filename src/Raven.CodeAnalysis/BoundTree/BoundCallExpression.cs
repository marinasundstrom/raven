using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

class BoundCallExpression : BoundExpression
{
    public IMethodSymbol Method { get; }
    public ImmutableArray<BoundExpression> Arguments { get; }

    public override ITypeSymbol Type => Method.ReturnType;

    public BoundCallExpression(IMethodSymbol method, ImmutableArray<BoundExpression> arguments)
    {
        Method = method;
        Arguments = arguments;
    }
}
