using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

// Expose synchronous iteration state before async capture analysis. IL-only loop
// temporaries cannot survive a MoveNext return; cleanup must respect suspension.
internal static class AsyncForLowerer
{
    public static BoundBlockStatement Rewrite(ISymbol owner, BoundBlockStatement body, bool useExceptionHandling = true)
        => (BoundBlockStatement)new Rewriter(owner, useExceptionHandling).VisitStatement(body)!;

    private sealed class Rewriter(ISymbol owner, bool useExceptionHandling) : BoundTreeRewriter
    {
        private readonly Compilation compilation = ((SourceAssemblySymbol)owner.ContainingAssembly).Compilation;
        private int ordinal;
        private ITypeSymbol Unit => compilation.GetSpecialType(SpecialType.System_Unit);
        private SourceLocalSymbol Temp(string name, ITypeSymbol type) => new(
            $"<{name}>__asyncFor{ordinal++}", type, true, owner, owner.ContainingType,
            owner.ContainingNamespace, [Location.None], []);
        private ILabelSymbol Label(string name) => new LabelSymbol(
            $"{name}__asyncFor{ordinal++}", owner, owner.ContainingType, owner.ContainingNamespace, [Location.None], []);
        private static BoundStatement Mark(ILabelSymbol label) => new BoundLabeledStatement(label, new BoundBlockStatement([]));
        private static BoundExpression Read(ILocalSymbol local) => new BoundLocalAccess(local);
        private static BoundStatement Declare(ILocalSymbol local, BoundExpression value, bool dispose = false)
            => new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, value)], isUsing: dispose);
        private BoundExpression Convert(BoundExpression value, ITypeSymbol type)
            => SymbolEqualityComparer.Default.Equals(value.Type, type) ? value
                : new BoundConversionExpression(value, type, compilation.ClassifyConversion(value.Type, type));
        private BoundStatement Assign(ILocalSymbol local, BoundExpression value)
            => new BoundAssignmentStatement(new BoundLocalAssignmentExpression(local, Read(local), value, Unit));
        private BoundExpression Binary(SyntaxKind kind, BoundExpression left, BoundExpression right)
        {
            if (!BoundBinaryOperator.TryLookup(compilation, kind, left.Type, right.Type, out var op))
                throw new InvalidOperationException("Missing bound async iteration operator.");
            return new BoundBinaryExpression(left, op, right);
        }
        private BoundExpression Number(int value, ITypeSymbol type)
            => Convert(new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value,
                compilation.GetSpecialType(SpecialType.System_Int32)), type);
        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;
        public override BoundNode? VisitLabeledStatement(BoundLabeledStatement node)
        {
            var labels = new List<ILabelSymbol>();
            BoundStatement current = node;
            while (current is BoundLabeledStatement labeled)
            {
                labels.Add(labeled.Label);
                current = labeled.Statement;
            }
            BoundStatement result;
            switch (current)
            {
                case BoundForStatement loop when loop.Iteration.Kind != ForIterationKind.Async:
                    result = RewriteFor(loop, labels);
                    break;
                case BoundWhileStatement loop:
                    result = RewriteLoop(loop.Condition, loop.Body, labels);
                    break;
                case BoundLoopStatement loop:
                    result = RewriteLoop(null, loop.Body, labels);
                    break;
                default:
                    return base.VisitLabeledStatement(node);
            }
            for (int i = labels.Count - 1; i >= 0; --i)
                result = new BoundLabeledStatement(labels[i], result);
            return result;
        }
        public override BoundNode? VisitForStatement(BoundForStatement node)
            => node.Iteration.Kind == ForIterationKind.Async ? base.VisitForStatement(node) : RewriteFor(node, []);

        public override BoundNode? VisitWhileStatement(BoundWhileStatement node)
            => RewriteLoop(node.Condition, node.Body, []);

        public override BoundNode? VisitLoopStatement(BoundLoopStatement node)
            => RewriteLoop(null, node.Body, []);

        // Keep enclosing loop transfers explicit too. Otherwise a structured
        // continue can bypass hoisted resource cleanup before the next acquisition.
        private BoundStatement RewriteLoop(BoundExpression? condition, BoundStatement body, List<ILabelSymbol> labels)
        {
            var begin = Label("loop_begin");
            var end = Label("loop_end");
            var statements = new List<BoundStatement> { Mark(begin) };
            if (condition is not null)
                statements.Add(new BoundConditionalGotoStatement(end, VisitExpression(condition)!, jumpIfTrue: false));
            statements.Add((BoundStatement)new Transfers(end, begin, labels).VisitStatement(VisitStatement(body)!)!);
            statements.Add(new BoundGotoStatement(begin, isBackward: true));
            statements.Add(Mark(end));
            return new BoundBlockStatement(statements);
        }

        private BoundStatement RewriteFor(BoundForStatement node, List<ILabelSymbol> sourceLabels)
        {
            var end = Label("for_end");
            var next = Label("for_next");
            var begin = Label("for_begin");
            var body = (BoundStatement)new Transfers(end, next, sourceLabels).VisitStatement(VisitStatement(node.Body)!)!;
            var prefix = new List<BoundStatement>();
            var step = new List<BoundStatement>();
            ImmutableArray<ILocalSymbol> disposables = [];
            BoundBlockStatement? cleanup = null;
            BoundExpression condition, current;
            switch (node.Iteration.Kind)
            {
                case ForIterationKind.Generic:
                case ForIterationKind.NonGeneric:
                    var get = node.Iteration.GetEnumeratorMethod!;
                    var iterator = Temp("iterator", get.ReturnType);
                    var collection = VisitExpression(node.Collection)!;
                    var optional = get.Parameters.Skip(get.IsExtensionMethod ? 1 : 0)
                        .Select(p => (BoundExpression)new BoundDefaultValueExpression(p.Type));
                    var acquire = get.IsExtensionMethod
                        ? new BoundInvocationExpression(get, new[] { collection }.Concat(optional), extensionReceiver: collection)
                        : new BoundInvocationExpression(get, optional, receiver: collection);
                    bool dispose = UseDisposalUtilities.SupportsUseDisposal(compilation, iterator.Type, preferAsync: false);
                    prefix.Add(Declare(iterator, acquire, dispose && !useExceptionHandling));
                    if (dispose && !useExceptionHandling)
                        disposables = [iterator];
                    if (dispose && useExceptionHandling && UseDisposalUtilities.TryResolveUseDisposeMethod(
                            compilation, iterator.Type, preferAsync: false, out var disposeMethod, out var useAwait))
                    {
                        cleanup = new BoundBlockStatement([new BoundExpressionStatement(
                            UseDisposalUtilities.CreateDisposeInvocationExpression(compilation, Read(iterator), disposeMethod!, useAwait))]);
                        if (iterator.Type.IsReferenceType)
                            cleanup = new BoundBlockStatement([new BoundIfStatement(
                                Binary(SyntaxKind.NotEqualsToken, Read(iterator), new BoundLiteralExpression(
                                    BoundLiteralExpressionKind.NullLiteral, null!, iterator.Type)), cleanup)]);
                    }
                    condition = new BoundInvocationExpression(node.Iteration.MoveNextMethod!, [], receiver: Read(iterator), requiresReceiverAddress: iterator.Type.IsValueType);
                    current = new BoundInvocationExpression(node.Iteration.CurrentGetter!, [], receiver: Read(iterator), requiresReceiverAddress: iterator.Type.IsValueType);
                    break;
                case ForIterationKind.Array:
                    var array = Temp("array", node.Iteration.ArrayType!);
                    var index = Temp("index", compilation.GetSpecialType(SpecialType.System_Int32));
                    prefix.Add(Declare(array, VisitExpression(node.Collection)!));
                    prefix.Add(Declare(index, Number(0, index.Type)));
                    var length = array.Type.GetMembers("Length").OfType<IPropertySymbol>().FirstOrDefault()
                        ?? array.Type.BaseType!.GetMembers("Length").OfType<IPropertySymbol>().First();
                    condition = Binary(SyntaxKind.LessThanToken, Read(index), new BoundMemberAccessExpression(Read(array), length));
                    current = new BoundArrayAccessExpression(Read(array), [Read(index)], node.Iteration.ElementType);
                    step.Add(Assign(index, Binary(SyntaxKind.PlusToken, Read(index), Number(1, index.Type))));
                    break;
                case ForIterationKind.Range:
                    var value = Temp("current", node.Iteration.ElementType);
                    var limit = Temp("end", value.Type);
                    var increment = Temp("step", value.Type);
                    prefix.Add(Declare(value, VisitExpression(node.Iteration.RangeStart!)!));
                    prefix.Add(Declare(limit, VisitExpression(node.Iteration.RangeEnd!)!));
                    prefix.Add(Declare(increment, VisitExpression(node.Iteration.RangeStep!)!));
                    var positive = Binary(SyntaxKind.GreaterThanToken, Read(increment), Number(0, value.Type));
                    var negative = Binary(SyntaxKind.LessThanToken, Read(increment), Number(0, value.Type));
                    var withinPositive = Binary(node.Iteration.RangeUpperExclusive ? SyntaxKind.LessThanToken : SyntaxKind.LessThanOrEqualsToken, Read(value), Read(limit));
                    var withinNegative = Binary(node.Iteration.RangeUpperExclusive ? SyntaxKind.GreaterThanToken : SyntaxKind.GreaterThanOrEqualsToken, Read(value), Read(limit));
                    condition = Binary(SyntaxKind.BarBarToken,
                        Binary(SyntaxKind.AmpersandAmpersandToken, positive, withinPositive),
                        Binary(SyntaxKind.AmpersandAmpersandToken, negative, withinNegative));
                    current = Read(value);
                    if (value.Type.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64)
                    {
                        BoundExpression Limit(bool maximum) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral,
                            value.Type.SpecialType == SpecialType.System_Int64
                                ? (object)(maximum ? long.MaxValue : long.MinValue)
                                : maximum ? int.MaxValue : int.MinValue, value.Type);
                        var overflow = new BoundIfExpression(positive,
                            Binary(SyntaxKind.GreaterThanToken, Read(value), Binary(SyntaxKind.MinusToken, Limit(true), Read(increment))),
                            Binary(SyntaxKind.LessThanToken, Read(value), Binary(SyntaxKind.MinusToken, Limit(false), Read(increment))));
                        step.Add(new BoundConditionalGotoStatement(end, overflow, jumpIfTrue: true));
                    }
                    step.Add(Assign(value, Binary(SyntaxKind.PlusToken, Read(value), Read(increment))));
                    break;
                default:
                    return base.VisitForStatement(node)! as BoundStatement ?? node;
            }
            prefix.Add(Mark(begin));
            prefix.Add(new BoundConditionalGotoStatement(end, condition, jumpIfTrue: false));
            var iteration = new List<BoundStatement>();
            if (node.Local is { } local)
                iteration.Add(Declare(local, Convert(current, local.Type)));
            iteration.Add(body);
            prefix.Add(new BoundBlockStatement(iteration));
            prefix.Add(Mark(next));
            prefix.AddRange(step);
            prefix.Add(new BoundGotoStatement(begin, isBackward: true));
            prefix.Add(Mark(end));
            if (cleanup is not null)
                return new BoundBlockStatement([prefix[0],
                    new BoundTryStatement(new BoundBlockStatement(prefix.Skip(1)), [], cleanup)]);
            return new BoundBlockStatement(prefix, disposables);
        }
    }

    private sealed class Transfers(ILabelSymbol end, ILabelSymbol next, List<ILabelSymbol> labels) : BoundTreeRewriter
    {
        private int depth;
        private bool Applies(ILabelSymbol? target) => target is null ? depth == 0 : labels.Contains(target);
        public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node) => node;
        public override BoundNode? VisitBreakStatement(BoundBreakStatement node)
            => Applies(node.TargetLabel) ? new BoundGotoStatement(end) : node;
        public override BoundNode? VisitContinueStatement(BoundContinueStatement node)
            => Applies(node.TargetLabel) ? new BoundGotoStatement(next) : node;
        public override BoundNode? VisitBreakExpression(BoundBreakExpression node)
            => Applies(node.TargetLabel) ? new BoundBlockExpression([new BoundGotoStatement(end)], node.Type, []) : node;
        public override BoundNode? VisitContinueExpression(BoundContinueExpression node)
            => Applies(node.TargetLabel) ? new BoundBlockExpression([new BoundGotoStatement(next)], node.Type, []) : node;
        public override BoundNode? VisitWhileStatement(BoundWhileStatement node)
        {
            ++depth;
            try
            {
                return base.VisitWhileStatement(node);
            }
            finally
            {
                --depth;
            }
        }
        public override BoundNode? VisitLoopStatement(BoundLoopStatement node)
        {
            ++depth;
            try
            {
                return base.VisitLoopStatement(node);
            }
            finally
            {
                --depth;
            }
        }
        public override BoundNode? VisitForStatement(BoundForStatement node)
        {
            ++depth;
            try
            {
                return base.VisitForStatement(node);
            }
            finally
            {
                --depth;
            }
        }
    }
}
