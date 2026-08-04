using System.Linq;

namespace Raven.CodeAnalysis;

internal partial class BoundInvocationExpression : BoundExpression
{
    public IMethodSymbol Method { get; }
    public IEnumerable<BoundExpression> Arguments { get; }
    public BoundExpression? Receiver { get; }
    public BoundExpression? ExtensionReceiver { get; }
    public bool RequiresReceiverAddress { get; }

    public BoundInvocationExpression(
        IMethodSymbol method,
        IEnumerable<BoundExpression> arguments,
        BoundExpression? receiver = null,
        BoundExpression? extensionReceiver = null,
        bool requiresReceiverAddress = false)
           : base(NullableMetadataFacts.GetReturnType(method), method, BoundExpressionReason.None)
    {
        Method = method;
        Arguments = arguments;
        Receiver = receiver;
        ExtensionReceiver = extensionReceiver;
        RequiresReceiverAddress = requiresReceiverAddress;
    }

    public override string ToString()
    {
        var receiverStr = Receiver is not null ? $"{Receiver}." : $"{Method.ContainingType.Name}.";
        var argsStr = string.Join(", ", Arguments.Select(a => a?.ToString() ?? "<missing>"));
        return $"{receiverStr}{Method.Name}({argsStr})";
    }
}
