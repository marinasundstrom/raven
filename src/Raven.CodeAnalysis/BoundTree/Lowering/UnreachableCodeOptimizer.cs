using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

/// <summary>
/// Removes statements proven unreachable within one lowered block. The
/// analysis is deliberately conservative across nested blocks and exception
/// regions.
/// </summary>
internal sealed class UnreachableCodeOptimizer : BoundTreeRewriter
{
    private UnreachableCodeOptimizer()
    {
    }

    public static BoundNode Rewrite(BoundNode node)
        => new UnreachableCodeOptimizer().Visit(node)!;

    public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
    {
        var rewritten = (BoundBlockStatement)base.VisitBlockStatement(node)!;
        var statements = rewritten.Statements.ToImmutableArray();
        if (statements.Length < 2)
            return rewritten;

        var reachable = BoundReachabilityAnalysis.Analyze(statements);
        if (reachable.All(static value => value))
            return rewritten;

        return rewritten.Update(
            statements.Where((_, index) => reachable[index]),
            rewritten.LocalsToDispose,
            rewritten.IntroduceILScope);
    }

    private static class BoundReachabilityAnalysis
    {
        public static ImmutableArray<bool> Analyze(ImmutableArray<BoundStatement> statements)
        {
            var labels = new Dictionary<ILabelSymbol, int>(ReferenceEqualityComparer.Instance);
            for (var index = 0; index < statements.Length; index++)
            {
                if (statements[index] is BoundLabeledStatement labeled)
                    labels[labeled.Label] = index;
            }

            var reachable = new bool[statements.Length];
            var work = new Queue<int>();
            Enqueue(0);

            // A source label can be entered from a containing statement region.
            // Keep it unless whole-method reachability proves otherwise.
            for (var index = 0; index < statements.Length; index++)
            {
                if (statements[index] is BoundLabeledStatement { Label.Locations: var locations } &&
                    locations.Any(static location => location is { IsInSource: true }))
                {
                    Enqueue(index);
                }
            }

            while (work.Count > 0)
            {
                var index = work.Dequeue();
                var statement = statements[index];

                switch (statement)
                {
                    case BoundGotoStatement @goto:
                        EnqueueTarget(@goto.Target);
                        break;
                    case BoundConditionalGotoStatement conditional:
                        EnqueueTarget(conditional.Target);
                        Enqueue(index + 1);
                        break;
                    case BoundBreakStatement { TargetLabel: { } breakTarget }:
                        EnqueueTarget(breakTarget);
                        break;
                    case BoundContinueStatement { TargetLabel: { } continueTarget }:
                        EnqueueTarget(continueTarget);
                        break;
                    case BoundReturnStatement or
                         BoundThrowStatement or
                         BoundBreakStatement or
                         BoundContinueStatement or
                         BoundYieldBreakStatement:
                        break;
                    default:
                        var transfers = TransferCollector.Collect(statement);
                        foreach (var target in transfers)
                            EnqueueTarget(target);
                        Enqueue(index + 1);
                        break;
                }
            }

            return reachable.ToImmutableArray();

            void EnqueueTarget(ILabelSymbol target)
            {
                if (labels.TryGetValue(target, out var targetIndex))
                    Enqueue(targetIndex);
            }

            void Enqueue(int index)
            {
                if ((uint)index >= (uint)reachable.Length || reachable[index])
                    return;

                reachable[index] = true;
                work.Enqueue(index);
            }
        }

        private sealed class TransferCollector : BoundTreeWalker
        {
            private readonly HashSet<ILabelSymbol> _targets = new(ReferenceEqualityComparer.Instance);

            public static IEnumerable<ILabelSymbol> Collect(BoundStatement statement)
            {
                var collector = new TransferCollector();
                collector.Visit(statement);
                return collector._targets;
            }

            public override void VisitConditionalGotoStatement(BoundConditionalGotoStatement node)
                => _targets.Add(node.Target);

            public override void VisitGotoStatement(BoundGotoStatement node)
                => _targets.Add(node.Target);

            public override void VisitBreakStatement(BoundBreakStatement node)
            {
                if (node.TargetLabel is { } target)
                    _targets.Add(target);
            }

            public override void VisitContinueStatement(BoundContinueStatement node)
            {
                if (node.TargetLabel is { } target)
                    _targets.Add(target);
            }
        }
    }
}
