using System;
using System.Linq;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitInvocationExpression(BoundInvocationExpression node)
    {
        var receiver = (BoundExpression?)VisitExpression(node.Receiver);
        var arguments = node.Arguments.Select(a => (BoundExpression)VisitExpression(a)!).ToArray();

        BoundExpression? extensionReceiver = null;
        if (node.ExtensionReceiver is not null)
        {
            extensionReceiver = ReferenceEquals(node.ExtensionReceiver, node.Receiver)
                ? receiver
                : (BoundExpression?)VisitExpression(node.ExtensionReceiver);
        }

        if (node.Method.IsExtensionMethod && extensionReceiver is not null)
        {
            var loweredArguments = new BoundExpression[arguments.Length + 1];
            loweredArguments[0] = extensionReceiver;
            Array.Copy(arguments, 0, loweredArguments, 1, arguments.Length);
            return new BoundInvocationExpression(node.Method, loweredArguments, receiver: null);
        }

        return new BoundInvocationExpression(node.Method, arguments, receiver, extensionReceiver);
    }
}

