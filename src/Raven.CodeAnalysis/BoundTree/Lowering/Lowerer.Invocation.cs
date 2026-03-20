using System;
using System.Collections.Generic;
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
        var staticQualifiedExtensionCall =
            node.Method.IsExtensionMethod &&
            node.ExtensionReceiver is null &&
            receiver is BoundTypeExpression &&
            arguments.Length == node.Method.Parameters.Length &&
            node.Method.Parameters.Length > 0;

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
            var lowered = new BoundInvocationExpression(
                node.Method,
                loweredArguments,
                receiver: null,
                requiresReceiverAddress: node.RequiresReceiverAddress);
            return HoistInvocationSubexpressionsIfNeeded(lowered);
        }

        if (staticQualifiedExtensionCall)
        {
            var lowered = new BoundInvocationExpression(
                node.Method,
                arguments,
                receiver: null,
                extensionReceiver: null,
                requiresReceiverAddress: node.RequiresReceiverAddress);
            return HoistInvocationSubexpressionsIfNeeded(lowered);
        }

        var invocation = new BoundInvocationExpression(
            node.Method,
            arguments,
            receiver,
            extensionReceiver,
            node.RequiresReceiverAddress);
        return HoistInvocationSubexpressionsIfNeeded(invocation);
    }

    private BoundExpression HoistInvocationSubexpressionsIfNeeded(BoundInvocationExpression invocation)
    {
        var receiver = invocation.Receiver;
        var arguments = invocation.Arguments.ToArray();
        if (!NeedsSubexpressionHoisting(receiver, arguments))
            return invocation;

        var compilation = GetCompilation();
        var statements = new List<BoundStatement>();

        BoundExpression? hoistedReceiver = null;
        if (receiver is not null)
            hoistedReceiver = HoistToTemp(receiver, "callReceiver", statements, compilation);

        var hoistedArguments = new BoundExpression[arguments.Length];
        for (var i = 0; i < arguments.Length; i++)
            hoistedArguments[i] = HoistToTemp(arguments[i], $"callArg{i}", statements, compilation);

        statements.Add(new BoundExpressionStatement(
            new BoundInvocationExpression(
                invocation.Method,
                hoistedArguments,
                hoistedReceiver,
                extensionReceiver: null,
                invocation.RequiresReceiverAddress)));

        return compilation.BoundNodeFactory.CreateBlockExpression(statements);
    }

    private static bool NeedsSubexpressionHoisting(BoundExpression? receiver, BoundExpression[] arguments)
    {
        if (receiver is BoundBlockExpression)
            return true;

        for (var i = 0; i < arguments.Length; i++)
        {
            if (arguments[i] is BoundBlockExpression)
                return true;
        }

        return false;
    }

    private BoundExpression HoistToTemp(
        BoundExpression expression,
        string nameHint,
        List<BoundStatement> statements,
        Compilation compilation)
    {
        var expressionType = expression.Type ?? compilation.ErrorTypeSymbol;
        var temp = CreateTempLocal(nameHint, expressionType, isMutable: false);
        statements.Add(new BoundLocalDeclarationStatement(
            ImmutableArray.Create(new BoundVariableDeclarator(temp, expression))));
        return new BoundLocalAccess(temp);
    }
}
