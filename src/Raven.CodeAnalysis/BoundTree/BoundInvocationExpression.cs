using System.Collections.Immutable;

namespace Raven.CodeAnalysis;

internal partial class BoundInvocationExpression : BoundExpression
{
    public IMethodSymbol Method { get; }
    public IEnumerable<BoundExpression> Arguments { get; }
    public BoundExpression? Receiver { get; }

    public BoundInvocationExpression(IMethodSymbol method, IEnumerable<BoundExpression> arguments, BoundExpression? receiver = null)
           : base(method.ReturnType, method, BoundExpressionReason.None)
    {
        Method = method;
        Arguments = arguments;
        Receiver = receiver;
    }

    public override string ToString()
    {
        var receiverStr = Receiver is not null ? $"{Receiver}." : $"{Method.ContainingType.Name}.";
        var argsStr = string.Join(", ", Arguments.Select(a => a.ToString()));
        return $"{receiverStr}{Method.Name}({argsStr})";
    }
}