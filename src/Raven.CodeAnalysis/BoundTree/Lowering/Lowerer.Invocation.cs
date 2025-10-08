using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitInvocationExpression(BoundInvocationExpression node)
    {
        var receiver = (BoundExpression?)VisitExpression(node.Receiver);
        var arguments = node.Arguments.Select(a => (BoundExpression)VisitExpression(a)!).ToArray();

        var receiverCameFromInvocation = node.ExtensionReceiver is not null &&
            ReferenceEquals(node.ExtensionReceiver, node.Receiver);

        BoundExpression? extensionReceiver = null;
        if (node.ExtensionReceiver is not null)
        {
            extensionReceiver = receiverCameFromInvocation
                ? receiver
                : (BoundExpression?)VisitExpression(node.ExtensionReceiver);
        }

        if (node.Method.IsExtensionMethod && extensionReceiver is not null)
        {
            var loweredArguments = new BoundExpression[arguments.Length + 1];
            loweredArguments[0] = extensionReceiver;
            Array.Copy(arguments, 0, loweredArguments, 1, arguments.Length);

            if (_loweringTrace is not null)
            {
                var compilation = GetCompilation();
                var argumentTypesBuilder = ImmutableArray.CreateBuilder<ITypeSymbol>(loweredArguments.Length);
                foreach (var argument in loweredArguments)
                {
                    argumentTypesBuilder.Add(argument.Type ?? compilation.ErrorTypeSymbol);
                }

                var receiverType = extensionReceiver.Type ?? compilation.ErrorTypeSymbol;
                var traceEntry = new ExtensionInvocationLoweringTrace(
                    _containingSymbol,
                    node.Method,
                    receiverType,
                    receiverCameFromInvocation,
                    argumentTypesBuilder.ToImmutable());

                _loweringTrace.RecordExtensionInvocation(traceEntry);
            }
            return new BoundInvocationExpression(
                node.Method,
                loweredArguments,
                receiver: null,
                requiresReceiverAddress: node.RequiresReceiverAddress);
        }

        return new BoundInvocationExpression(
            node.Method,
            arguments,
            receiver,
            extensionReceiver,
            node.RequiresReceiverAddress);
    }
}

